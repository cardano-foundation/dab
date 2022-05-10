
module ChainWatcher.Server where


import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Reader

import Data.Aeson (encode, ToJSON(toJSON))
import Data.ByteString.Builder (intDec, lazyByteString, string8)
import Data.Map (Map)
import Data.OpenApi (OpenApi)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.UUID.V4 (nextRandom)

import qualified Data.ByteString.Lazy
import qualified Data.Map
import qualified Data.Set
import qualified Data.Yaml
import qualified Pipes

import Network.Wai (Middleware)
import Network.Wai.EventSource
  ( ServerEvent (CloseEvent, CommentEvent, ServerEvent)
  )
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.Gzip (def, gzip)
import Servant
import Servant.API.EventStream
import Servant.OpenApi ()
import Servant.Swagger.UI.Blaze
import Servant.Swagger.UI.ReDoc.Blaze

import ChainWatcher.Api
import ChainWatcher.Lens
import ChainWatcher.OpenApi
import ChainWatcher.Types

-- * Core server

data ServerState = ServerState
  { serverStateClients      :: TVar (Map ClientId ClientState)
  , serverStateRequestQueue :: TQueue RequestDetail
  }

type AppM = ReaderT ServerState Handler

server :: ServerT API AppM
server =
       pure True
  :<|> handleSSE
  :<|> handleClientsApi

handleClientsApi :: ServerT ClientsAPI AppM
handleClientsApi =
       handleNewClient
  :<|> handleRemoveClient
  :<|> handleNewRequest
  :<|> handleEvents

handleNewClient :: AppM ClientId
handleNewClient = do
  tclients <- asks serverStateClients
  liftIO $ do
    cId <- ClientId <$> nextRandom
    atomically $ modifyTVar tclients (Data.Map.insert cId newClientState)
    pure cId

handleRemoveClient :: ClientId -> AppM ()
handleRemoveClient cid = do
  tclients <- asks serverStateClients
  liftIO $ atomically $ modifyTVar tclients (Data.Map.delete cid)

handleNewRequest :: ClientId -> Request -> AppM Integer
handleNewRequest cid req = do
  tclients <- asks serverStateClients

  t <- liftIO getPOSIXTime
  let rd = RequestDetail {
         requestDetailRequestId = 0
       , requestDetailClientId = cid
       , requestDetailRequest = req
       , requestDetailTime = t
       }

  res <- liftIO $ atomically $ do
    clients <- readTVar tclients
    case Data.Map.lookup cid clients of
      Nothing -> pure Nothing
      Just c -> do
        let nextId = succ $ clientStateLastId c
            nextReq = rd { requestDetailRequestId = nextId }
            newC = c {
                clientStateRequests =
                  Data.Set.insert
                    nextReq
                    (clientStateRequests c)

              , clientStateLastId = nextId
              }
        modifyTVar tclients (Data.Map.adjust (pure newC) cid)
        pure $ Just nextReq

  maybe
    (throwError err404)
    (\r -> do
       qreqs <- asks serverStateRequestQueue
       liftIO $ atomically $ writeTQueue qreqs r
       return (requestDetailRequestId r)
    )
    res

handleEvents :: ClientId -> Maybe Bool -> AppM [EventDetail]
handleEvents cid longPoll = do
  tclients <- asks serverStateClients
  res <- liftIO $ atomically $ do
    clients <- readTVar tclients
    case Data.Map.lookup cid clients of
      Nothing -> pure Nothing
      Just c -> do
        let (!evts, newC) = takeEvents c
        when (longPoll == Just True) $ guard (not . null $ evts)
        modifyTVar tclients (Data.Map.adjust (pure newC) cid)
        pure $ Just $ clientStatePastEvents newC

  maybe (throwError err404) pure res

handleSSE :: ClientId -> AppM EventSourceHdr
handleSSE cid = do
  tclients <- asks serverStateClients
  t <- liftIO getPOSIXTime
  res <- liftIO $ atomically $ do
    clients <- readTVar tclients
    case Data.Map.lookup cid clients of
      Nothing -> pure Nothing
      Just c -> do
        let rd = RequestDetail {
                     requestDetailRequestId = 0
                   , requestDetailClientId = cid
                   , requestDetailRequest = Recurring Ping
                   , requestDetailTime = t
                   }
        let nextId = succ $ clientStateLastId c
            nextReq = rd { requestDetailRequestId = nextId }
            newC = c {
                clientStateRequests =
                  Data.Set.insert
                  nextReq
                  (clientStateRequests c)

              , clientStateLastId = nextId
              }

        modifyTVar tclients (Data.Map.adjust (pure newC) cid)
        pure $ Just nextReq

  case res of
    Nothing -> throwError err404
    Just req -> do
      qreqs <- asks serverStateRequestQueue
      liftIO $ atomically $ writeTQueue qreqs req

      return $ eventSource $ do
          Pipes.yield (CommentEvent (string8 "hi"))
          forever $ do
            evts <- liftIO $ atomically $ do
              clients <- readTVar tclients
              case Data.Map.lookup cid clients of
                Nothing -> pure $ pure CloseEvent
                Just c -> do
                  let (evts, newC) = takeEvents c
                  modifyTVar tclients (Data.Map.adjust (pure newC) cid)
                  pure $ map eventDetailAsServerEvent evts
            forM_ evts Pipes.yield
            liftIO $ do
              Control.Concurrent.threadDelay 1_000_000

-- * Docs

type DocsAPI =
       SwaggerSchemaUI "swagger-ui" "swagger.json" OpenApi
  :<|> SwaggerSchemaUI "redoc" "swagger.json" OpenApi
  :<|> "swagger.yaml" :> Get '[YAML] OpenApi

data YAML

instance Accept YAML where
  contentType _ = "text/yaml"

instance {-# OVERLAPPABLE #-} ToJSON a => MimeRender YAML a where
  mimeRender _ x = Data.ByteString.Lazy.fromStrict $ Data.Yaml.encode x

docServer :: Server DocsAPI
docServer =
            swaggerSchemaUIServer "ChainWatcher API - Swagger UI" chainwatcherSwagger
       :<|> redocSchemaUIServer "ChainWatcher API - ReDoc" chainwatcherSwagger
       :<|> pure chainwatcherSwagger

type CombinedAPI =
       API
  :<|> DocsAPI
  :<|> "demo" :> Raw

-- * Combined

combinedServer
  :: TVar (Map ClientId ClientState)
  -> TQueue RequestDetail
  -> Server CombinedAPI
combinedServer clients qreqs =
  hoistServer
    api
    (flip
      Control.Monad.Reader.runReaderT
        (ServerState clients qreqs))
    server
  :<|> docServer
  :<|> serveDirectoryWebApp "static"

runServer
  :: TVar (Map ClientId ClientState)
  -> TQueue RequestDetail -> IO ()
runServer clients qreqs = do
  run 8282
    $ simpleCors
    $ gzip def
    $ headers
    $ serve (Proxy @CombinedAPI)
    $ combinedServer clients qreqs
  where
      -- headers required for SSE to work through nginx
      -- not required if using warp directly
      headers :: Middleware
      headers = addHeaders [
                             ("Cache-Control", "no-cache")
                           ]

-- * Server Sent Events

eventDetailAsServerEvent :: EventDetail -> ServerEvent
eventDetailAsServerEvent ed =
  ServerEvent
    (Just $ string8 $ eventName $ ed ^. event)
    (Just $ intDec . (\(EventId i _uuid) -> i) $ ed ^. eventId)
    [lazyByteString $ encode $ toJSON ed]

eventName :: Event -> String
eventName (Pong _)                   = "Pong"
eventName (SlotReached _)            = "SlotReached"
eventName (UtxoSpent _ _)            = "UtxoSpent"
eventName (UtxoProduced _ _)         = "UtxoProduced"
eventName (TransactionConfirmed _)   = "TransactionConfirmed"
eventName (AddressFundsChanged _)    = "AddressFundsChanged"
eventName (Rollback evt)             = "Rollback" ++ (eventName evt)
