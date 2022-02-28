{-# LANGUAGE NumericUnderscores #-}
module ChainWatcher where


import Control.Monad
import Control.Monad.Reader hiding (ask, Reader, runReader, fix, liftIO)
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Reader hiding (asks)
import Control.Monad.Freer.Writer
import Control.Monad.Freer.State
import Control.Monad.Freer.Log
import Control.Monad.Freer.Time
import Control.Monad.IO.Class (liftIO)

import Colog.Core.IO (logStringStdout)

import Data.Time.Clock.POSIX
import qualified Data.Text
import Data.Function (fix)
import qualified Data.Map

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async

import Blockfrost.Freer.Client hiding (api)
import Control.Lens

import Text.Pretty.Simple

import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set
import Data.Text (Text)

import Servant hiding (throwError)
import qualified Servant
import Servant.API.EventStream
import qualified Network.Wai.Handler.Warp as Warp
import           Network.Wai.EventSource        ( ServerEvent(..) )
import Network.Wai (Middleware)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.Gzip (gzip)
import Network.Wai.Middleware.Cors

import qualified Pipes                    as P

import           Data.ByteString.Builder

import Data.UUID.V4

import ChainWatcher.Types
main :: IO ()
main = do
  (qreqs, qevts) <- (,) <$> newTQueueIO <*> newTQueueIO
  {--
  let
      reqMin1 = AddressFundsRequest "addr_test1wpjduy92cj4dqzs6y5refxphskru3kfgyhchyg7u7nadt8qqfddxd"
      reqMin2 = AddressFundsRequest "addr_test1wphyve8r76kvfr5yn6k0fcmq0mn2uf6c6mvtsrafmr7awcg0vnzpg"
      -- my testnet addr
      --    "addr_test1vp9aktzhltgk4rf72l7nr7jaarqvzdfg84nukle8qkh7g3skpzhch"
  --}

  tclients <- newTVarIO mempty
  _apiAsync <- async $ mains tclients qreqs

  _pumpAsync <- async $ forever $ do
    atomically $ do
      evts <- flushTQueue qevts
      clients <- readTVar tclients
      let newClients = foldl (\cs e -> routeEvent e cs) clients evts
      writeTVar tclients newClients
    Control.Concurrent.threadDelay 1_000_000
  runWatcher qreqs qevts

runWatcher :: TQueue RequestDetail -> TQueue EventDetail -> IO ()
runWatcher qreqs qevts = fix $ \loop -> do
  prj <- projectFromEnv
  startBlock' <- runBlockfrost prj getLatestBlock
  -- should look-up N-depth previous blocks and use Nth as startBlock
  -- in case that the one we get from getLatestBlock disappears
  case startBlock' of
    Left _e -> loop -- error $ show ("Blockfrost error", e)
    Right startBlock -> do
      handleBlockfrostClient <- defaultBlockfrostHandler
      res <- runM
        . runError @WatcherError
        . runLogAction (contramap Data.Text.unpack logStringStdout)
        . runTime
        . runState @Block startBlock
        . runState @[Block] (pure startBlock)
        . runState @(Set RequestDetail) mempty
        . runReader @Int 10
        . handleBlockfrostClient
        . runReader @(TQueue RequestDetail) qreqs
        . runReader @(TQueue EventDetail) qevts
        . handleWatchSource
        $ do
            watch

      case res of
        Right _ -> pure ()
        Left (e :: WatcherError) -> do
          putStrLn $ "Exited with error " ++ show e
          putStrLn "Restarting"
          Control.Concurrent.threadDelay 10_000_000
          loop

maxRollbackSize :: Int
maxRollbackSize = 2160

showText :: Show a => a -> Text
showText = Data.Text.pack . show

data WatcherError =
    RuntimeError Text
  | APIClientError BlockfrostError
  deriving (Eq, Show)

rethrow :: (Member (Error WatcherError) effs) => BlockfrostError -> Eff effs a
rethrow = throwError . APIClientError

watch :: forall m effs a .
  ( LastMember m effs
  , LastMember m (Writer [(RequestDetail, EventDetail)] : effs) -- due to nested writer
  , MonadIO m
  , Members ClientEffects effs
  , Member (State (Set RequestDetail)) effs
  , Member (State Block) effs
  , Member (State [Block]) effs
  , Member WatchSource effs
  , Member (Log Text) effs
  , Member Time effs
  , Member (Error WatcherError) effs
  )
 => Eff effs a
watch = do
  startBlock <- get @Block
  logs @Text $ "Watcher thread started at block " <> (startBlock ^. hash . coerced)


  forever $ do
    newReqs <- getRequests
    unless (Data.Set.null newReqs) $ logs $ "New requests " <> showText newReqs
    modify @(Set RequestDetail) (Data.Set.union newReqs)

    currentBlock <- get @Block
    next <- tryError $ getNextBlocks (Right $ currentBlock ^. hash)
    case next of
      Left BlockfrostNotFound -> do
        logs @Text $ "Block disappeared " <> (currentBlock ^. hash . coerced)
        prev <- get @[Block]
        -- find previous existing block and rollback to it
        let findExisting [] _ = throwError $ RuntimeError "Disaster, rollback too large and cannot recover"
            findExisting (b:bs) dropped = do
              res <- tryError $ getBlock (Right $ b ^. hash)
              case res of
                Left BlockfrostNotFound -> findExisting bs (b:dropped)
                Left e -> rethrow e
                Right found -> pure (found, b:bs, dropped)

        (found, new, dropped) <- findExisting prev []
        logs @Text $ "Found previous existing block " <> (found ^. hash . coerced)
        put @Block found
        put @[Block] new
        logs @Text $ "Dropped " <> (showText $ length dropped) <> " blocks"
        -- onRollback
        pure ()

      Left e -> rethrow e
      Right [] -> pure ()
      Right bs -> do
        put @Block $ Prelude.last bs
        modify @[Block] $ \xs -> take maxRollbackSize $ reverse bs ++ xs

        -- Process new blocks
        -- this whole thing shouldn't produce events until it fully succeeds
        -- so we run writer
        res <- tryError @BlockfrostError
                $ fmap snd
                $ runWriter @[(RequestDetail, EventDetail)]
                $ do
          forM_ bs $ \blk -> do
            blockSlot <- case blk ^. slot of
              Just s -> pure s
              Nothing -> throwError $ BlockfrostError "Block with no slot"

            logs @Text $ "Processing new block " <> (blk ^. hash . coerced)
            addrsTxs <- getBlockAffectedAddresses (Right $ blk ^. hash)
            -- [(Address, [TxHash])]
            -- check vs tracked txs and addrs
            let addrs = Data.Set.fromList $ map fst addrsTxs
                addrTxMap = Data.Map.fromList addrsTxs

            reqs <- get @(Set RequestDetail)
            let newEventDetail rd ptime evt = EventDetail
                        { eventDetailEventId = requestDetailRequestId rd
                        , eventDetailClientId = requestDetailClientId rd
                        , eventDetailEvent = evt
                        , eventDetailTime = ptime
                        }
                handleRequest rd evt = do
                  getTime >>= \t -> tell [(rd, newEventDetail rd t evt)]

            forM (Data.Set.toList reqs) $ \req -> do
              case unRecurring $ req ^. request of
                AddressFundsRequest addr | addr `Data.Set.member` addrs -> do
                  handleRequest req $ AddressFundsChanged addr

                Ping -> do
                  handleRequest req $ Pong blockSlot

                SlotRequest s | s >= blockSlot -> do
                  handleRequest req $ SlotReached blockSlot

                UtxoProducedRequest addr | addr `Data.Set.member` addrs -> do
                  case Data.Map.lookup addr addrTxMap of
                    Nothing -> pure () -- can't happen but we should log it
                    Just txs -> do
                      utxoProducing <- forM txs $ \tx -> do
                        utxos <- getTxUtxos tx
                        let relevant = filter ((== addr) . view address) (utxos ^. outputs)
                        pure $ case relevant of
                          [] -> Nothing
                          _  -> pure tx
                      logs $ "Utxos producing txs " <> showText utxoProducing
                      case catMaybes $ utxoProducing of
                        [] -> pure ()
                        ptxs -> handleRequest req $ UtxoProduced addr ptxs


                _ -> pure ()

        case res of
          Left e -> rethrow e
          Right handled -> do
            logs $ "Produced " <> (showText $ length handled) <> " events"
            let handledReqs = Data.Set.fromList $ map fst handled

            modify @(Set RequestDetail)
              (flip Data.Set.difference
                 (Data.Set.filter (not . recurring) handledReqs))

            forM_ (map snd handled) $ \evt -> produceEvent evt

    liftIO $ do
      Control.Concurrent.threadDelay 20_000_000

-- | Run server
handleWatchSource
  :: forall a m effs
  . ( LastMember m effs
    , Member (Reader (TQueue EventDetail)) effs
    , Member (Reader (TQueue RequestDetail)) effs
    , MonadIO m )
  => Eff (WatchSource ': effs) a
  -> Eff effs a
handleWatchSource = interpret $ \case
  GetRequests -> ask >>= fmap Data.Set.fromList . liftIO . atomically . flushTQueue
  ProduceEvent e -> ask >>= \q -> liftIO . atomically $ writeTQueue q e


type API =
       "healthcheck" :> Get '[JSON] Bool
  :<|> "sse" :> Capture "client_id" ClientId :> ServerSentEvents
  :<|> "clients" :> ClientsAPI
  :<|> "demo" :> Raw

type ClientsAPI =
        "new"
    :> Post '[JSON] ClientId
   :<|>
        "remove"
    :> Capture "client_id" ClientId
    :> Post '[JSON] ()
   :<|>
        "request"
    :> Capture "client_id" ClientId
    :> ReqBody '[JSON] Request
    :> Post '[JSON] Integer

api :: Proxy API
api = Proxy

data ServerState = ServerState
  { serverStateClients :: TVar Clients
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
    (Servant.throwError err404)
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
    Nothing -> Servant.throwError err404
    Just req -> do
      qreqs <- asks serverStateRequestQueue
      liftIO $ atomically $ writeTQueue qreqs req

      return $ eventSource $ do
          P.yield (CommentEvent (string8 "hi"))
          forever $ do
            evts <- liftIO $ atomically $ do
              clients <- readTVar tclients
              case Data.Map.lookup cid clients of
                Nothing -> pure $ pure CloseEvent
                Just c -> do
                  let (evts, newC) = takeEvents c
                  modifyTVar tclients (Data.Map.adjust (pure newC) cid)
                  pure $ map eventDetailAsServerEvent evts
            forM_ evts P.yield
            liftIO $ do
              Control.Concurrent.threadDelay 1_000_000

mains :: TVar Clients -> TQueue RequestDetail -> IO ()
mains clients qreqs = do
  Warp.run 8282
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
eventDetailAsServerEvent ed = addId (eventDetailEventId ed) $ eventAsServerEvent (eventDetailEvent ed)
  where
    addId eid x@ServerEvent{} = x { Network.Wai.EventSource.eventId = Just $ integerDec eid }
    addId _ x = x

eventAsServerEvent :: Event -> ServerEvent
eventAsServerEvent (Pong (Slot s)) = ServerEvent
  (Just $ string8 "Pong")
  Nothing
  [integerDec s]
eventAsServerEvent (SlotReached (Slot s)) = ServerEvent
  (Just $ string8 "SlotReached")
  Nothing
  [integerDec s]
eventAsServerEvent (UtxoSpent txo spendingTx) = ServerEvent
  (Just $ string8 "UtxoSpent")
  Nothing
  [buildTxo txo, buildTx spendingTx]
eventAsServerEvent (UtxoProduced addr producingTxs) = ServerEvent
  (Just $ string8 "UtxoProduced")
  Nothing
  (buildAddr addr : map buildTx producingTxs)
eventAsServerEvent (TransactionConfirmed tx) = ServerEvent
  (Just $ string8 "TransactionConfirmed")
  Nothing
  [buildTx tx]
eventAsServerEvent (TransactionTentative tx confirms) = ServerEvent
  (Just $ string8 "TransactionTentative")
  Nothing
  [buildTx tx, integerDec confirms]
eventAsServerEvent (AddressFundsChanged addr) = ServerEvent
  (Just $ string8 "AddressFundsChanged")
  Nothing
  [buildAddr addr]
eventAsServerEvent (Rollback evt) =
  let origEvent = eventAsServerEvent evt
  in origEvent
    { eventName = Just (string8 "Rollback") <> eventName origEvent }

buildAddr :: Address -> Builder
buildAddr = string8 . Data.Text.unpack . unAddress

buildTx :: Tx -> Builder
buildTx = string8 . Data.Text.unpack . unTxHash

buildTxo :: TxOutRef -> Builder
buildTxo (tx, idx) = buildTx tx <> "#"<> integerDec idx
