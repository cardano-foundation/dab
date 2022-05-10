
module ChainWatcher where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.AcidState
import Control.Monad.Freer.Error
import Control.Monad.Freer.Fresh
import Control.Monad.Freer.Log
import Control.Monad.Freer.Reader hiding (asks)
import Control.Monad.Freer.State
import Control.Monad.Freer.Time
import Control.Monad.Freer.Writer
import Control.Monad.Freer.UUID
import Control.Monad.IO.Class (MonadIO (liftIO))

import Colog.Core.IO (logStringStdout)

import Data.Function (fix)
import Data.Map (Map)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set)
import Data.Text (Text)

import qualified Data.Map
import qualified Data.Set
import qualified Data.Text

import Blockfrost.Freer.Client hiding (api)

import ChainWatcher.Lens
import ChainWatcher.Server
import ChainWatcher.State
import ChainWatcher.Types

main :: IO ()
main = do
  (qreqs, qevts) <- (,) <$> newTQueueIO <*> newTQueueIO

  tclients <- newTVarIO mempty
  _apiAsync <- forkIO $ runServer tclients qreqs

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

  let delayAndLoop = do
        putStrLn "Restarting"
        Control.Concurrent.threadDelay 10_000_000
        loop

  case startBlock' of
    Left e -> do
      putStrLn $ "Can't fetch latest block, error was " ++ show e
      delayAndLoop
    Right startBlock -> do
      blkAcid <- openLocalState ([startBlock])
      handleBlockfrostClient <- defaultBlockfrostHandler
      res <- runM
        . runError @WatcherError
        . runLogAction (contramap Data.Text.unpack logStringStdout)
        . runTime
        . evalAcidState @[Block] blkAcid QueryState WriteState
        . evalState @(Set RequestDetail) mempty
        . evalState @(Map TxOutRef Address) mempty
        . evalFresh 0
        . handleUUIDEffect
        . handleBlockfrostClient
        . runReader @(TQueue RequestDetail) qreqs
        . runReader @(TQueue EventDetail) qevts
        . handleWatchSource
        $ do
            watch

      case res of
        Right (Left (be :: BlockfrostError)) -> do
          putStrLn $ "Exited with blockfrost error" ++ show be
          delayAndLoop
        Right _ -> pure ()
        Left (e :: WatcherError) -> do
          putStrLn $ "Exited with error " ++ show e
          delayAndLoop

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
  , Member (State [Block]) effs
  , Member (State (Map TxOutRef Address)) effs
  , Member WatchSource effs
  , Member (Log Text) effs
  , Member Time effs
  , Member UUIDEffect effs
  , Member Fresh effs
  , Member (Error WatcherError) effs
  )
 => Eff effs a
watch = do
  startBlock <- head <$> get @[Block]
  logs @Text $ "Watcher thread started at block " <> (startBlock ^. hash . coerced)

  forever $ do
    currentBlock <- head <$> get @[Block]

    newReqs <- getRequests
    unless (Data.Set.null newReqs) $ do
      logs $ "New requests " <> showText newReqs
      handled <-
        fmap snd
        $ runWriter @[(RequestDetail, EventDetail)]
        $ forM_ (Data.Set.toList newReqs) $ \req -> case unRecurring $ req ^. request of
            TransactionStatusRequest tx -> do
              txBlk <- tryError $ do
                txInfo <- getTx tx
                getBlock (Left $ txInfo ^. blockHeight)

              case txBlk of
                Left BlockfrostNotFound -> pure ()
                Left e -> rethrow e
                Right blk -> do
                  handleRequest req blk $ TransactionConfirmed tx

            -- look up the address holding the utxo so we can track it in block processing loop
            UtxoSpentRequest txoutref@(tx, txoutindex) -> do
              etx <- tryError $ getTxUtxos tx
              case etx of
                Left BlockfrostNotFound -> logs @Text $ "Transaction not found for TxOutRef " <> showText txoutref
                Left e -> rethrow e
                Right utxos -> do
                  let relevant = filter ((== txoutindex) . view outputIndex) (utxos ^. outputs)
                  case relevant of
                    [x] -> do
                      let addr = x ^. address
                      logs @Text $ "Adding utxo address to map of watched addresses " <> showText addr
                      modify @(Map TxOutRef Address) $ Data.Map.insert txoutref addr

                    _ -> logs $ "Transaction output not found" <> showText txoutref

            _ -> pure ()

      logs $ "Instantly handled " <> (showText $ length handled) <> " requests"
      forM_ (map snd handled) produceEvent

      modify @(Set RequestDetail) $
        Data.Set.union
          (Data.Set.difference
            newReqs
            (Data.Set.fromList $ map fst handled)
          )

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
                Left e                  -> rethrow e
                Right found             -> pure (found, b:bs, dropped)

        (found, new, dropped) <- findExisting prev []
        logs @Text $ "Found previous existing block " <> (found ^. hash . coerced)
        put @[Block] new
        logs @Text $ "Dropped " <> (showText $ length dropped) <> " blocks"
        -- onRollback
        pure ()

      Left e -> rethrow e
      Right [] -> logs @Text "No new blocks"
      Right newBlocks -> do
        -- Process new blocks
        -- this whole thing shouldn't produce events until it fully succeeds
        -- so we run writer
        res <- tryError @BlockfrostError
                $ fmap snd
                $ runWriter @[(RequestDetail, EventDetail)]
                $ do
          forM_ newBlocks $ \blk -> do
            blockSlot <- case blk ^. slot of
              Just s  -> pure s
              Nothing -> throwError $ RuntimeError "Block with no slot"

            logs @Text $ "Processing new block " <> (blk ^. hash . coerced)
            addrsTxs <- getBlockAffectedAddresses (Right $ blk ^. hash)
            -- [(Address, [TxHash])]
            -- check vs tracked txs and addrs
            let addrs = Data.Set.fromList $ map fst addrsTxs
                addrTxMap = Data.Map.fromList addrsTxs
                blockTxHashes = Data.Set.fromList $ concatMap snd addrsTxs

            reqs <- get @(Set RequestDetail)

            forM (Data.Set.toList reqs) $ \req -> do
              let handle = handleRequest req blk
              case unRecurring $ req ^. request of
                AddressFundsRequest addr | addr `Data.Set.member` addrs -> do
                  handle $ AddressFundsChanged addr

                Ping -> do
                  handle $ Pong blockSlot

                SlotRequest s | s >= blockSlot -> do
                  handle $ SlotReached blockSlot

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
                        []   -> pure ()
                        ptxs -> handle $ UtxoProduced addr ptxs

                UtxoSpentRequest txoutref@(txOutHash, txOutIndex)  -> do
                  txOutRefAddrs <- get @(Map TxOutRef Address)
                  case Data.Map.lookup txoutref txOutRefAddrs of
                    Nothing -> do
                      logs $ "UtxoSpentRequest with no mapping to address " <> showText txoutref
                      pure ()
                    Just addr -> do
                      case Data.Map.lookup addr addrTxMap of
                        Nothing -> pure ()
                        Just txs -> do
                          forM_ txs $ \tx -> do
                            utxos <- getTxUtxos tx
                            let relevant = filter (
                                  \i ->     i ^. outputIndex == txOutIndex
                                         && i ^. txHash == txOutHash
                                  ) (utxos ^. inputs)
                            case relevant of
                              [_x] -> do
                                logs $ "Utxo spending tx " <> showText tx <> " txo: " <> showText txoutref
                                handle $ UtxoSpent txoutref tx
                              _ -> pure ()

                TransactionStatusRequest tx | tx `Data.Set.member` blockTxHashes ->
                  handle $ TransactionConfirmed tx

                _ -> pure ()

        case res of
          Left e -> do
            logs $ "Error caught during new block processing loop " <> (showText e)
          Right handled -> do
            modify @[Block] $ \xs -> take maxRollbackSize $ reverse newBlocks ++ xs

            logs $ "Produced " <> (showText $ length handled) <> " events"
            let handledReqs = Data.Set.fromList $ map fst handled

            modify @(Set RequestDetail)
              (flip Data.Set.difference
                 (Data.Set.filter (not . recurring) handledReqs))

            let spentTxOutRefs =
                    catMaybes
                  $ map (preview _UtxoSpentRequest . view request)
                  $ map fst handled
            modify @(Map TxOutRef Address)
              $ Data.Map.filterWithKey (\k _ -> not $ k `elem` spentTxOutRefs)

            forM_ (map snd handled) $ \evt -> produceEvent evt

    liftIO $ do
      Control.Concurrent.threadDelay 20_000_000

newEventDetail
  :: RequestDetail
  -> POSIXTime
  -> EventId
  -> Block
  -> Event
  -> EventDetail
newEventDetail rd ptime eid blk evt = EventDetail
  { eventDetailRequestId = requestDetailRequestId rd
  , eventDetailEventId = eid
  , eventDetailClientId = requestDetailClientId rd
  , eventDetailEvent = evt
  , eventDetailTime = ptime
  , eventDetailAbsSlot = fromMaybe (error "Block with no slot") $ blk ^. slot
  , eventDetailBlock = fromMaybe (error "Block with no height") $ blk ^. height
  }

handleRequest
  :: ( Member Time effs
     , Member (Writer [(RequestDetail, EventDetail)]) effs
     , Member UUIDEffect effs
     , Member Fresh effs
     )
  => RequestDetail
  -> Block
  -> Event
  -> Eff effs ()
handleRequest rd blk evt = do
  freshId <- fresh
  uuid <- uuidNextRandom
  getTime >>= \t -> tell
    $ [(rd , newEventDetail rd t (EventId freshId uuid) blk evt)]

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
