
module ChainWatcher.Server where


import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Reader

import Data.Aeson (encode, toJSON)
import Data.ByteString.Builder (integerDec, lazyByteString, string8)
import Data.Map (Map)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.UUID.V4 (nextRandom)

import qualified Data.Map
import qualified Data.Set
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

import ChainWatcher.Api
import ChainWatcher.Types

data ServerState = ServerState
  { serverStateClients      :: TVar (Map ClientId ClientState)
  , serverStateRequestQueue :: TQueue RequestDetail
  }

type AppM = ReaderT ServerState Handler

-- and also requests queue
server :: ServerT API AppM
server =
       pure True
  :<|> handleSSE
  :<|> handleClientsApi
  :<|> serveDirectoryWebApp "static"

handleClientsApi :: ServerT ClientsAPI AppM
handleClientsApi =
       handleNewClient
  :<|> handleRemoveClient
  :<|> handleNewRequest

handleNewClient :: AppM ClientId
handleNewClient = do
  tclients <- asks serverStateClients
  liftIO $ do
    uuid <- nextRandom
    atomically $ modifyTVar tclients (Data.Map.insert uuid newClientState)
    pure uuid

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

runServer
  :: TVar (Map ClientId ClientState)
  -> TQueue RequestDetail -> IO ()
runServer clients qreqs = do
  run 8282
    $ simpleCors
    $ gzip def
    $ headers
    $ serve api
    $ hoistServer
        api
        (flip Control.Monad.Reader.runReaderT (ServerState clients qreqs))
        server
  where
      -- headers required for SSE to work through nginx
      -- not required if using warp directly
      headers :: Middleware
      headers = addHeaders [
                             ("Cache-Control", "no-cache")
                           ]

eventDetailAsServerEvent :: EventDetail -> ServerEvent
eventDetailAsServerEvent ed =
  ServerEvent
    (Just $ string8 $ eventName $ ed ^. event)
    (Just $ integerDec $ ed ^. eventId)
    [lazyByteString $ encode $ toJSON ed]

eventName :: Event -> String
eventName (Pong _)                   = "Pong"
eventName (SlotReached _)            = "SlotReached"
eventName (UtxoSpent _ _)            = "UtxoSpent"
eventName (UtxoProduced _ _)         = "UtxoProduced"
eventName (TransactionTentative _ _) = "TransactionTentative"
eventName (TransactionConfirmed _)   = "TransactionConfirmed"
eventName (AddressFundsChanged _)    = "AddressFundsChanged"
eventName (Rollback evt)             = "Rollback" ++ (eventName evt)
