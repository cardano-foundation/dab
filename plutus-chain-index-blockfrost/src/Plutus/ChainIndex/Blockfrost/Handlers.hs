{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
{-| Handlers for the 'ChainIndexQueryEffect' and the 'ChainIndexControlEffect' -}
module Plutus.ChainIndex.Blockfrost.Handlers
    ( handleQuery
    , handleControl
    , restoreStateFromDb
    , getResumePoints
    , ChainIndexState
    ) where

import Cardano.Api qualified as C
import Control.Applicative (Const (..))
import Control.Lens (Lens', _Just, ix, view, (^?), (^.))
import Control.Monad.Freer (Eff, Member, Members, type (~>), LastMember)
import Control.Monad.Freer.Error (Error, throwError, catchError)
import Control.Monad.Freer.Extras.Beam (BeamEffect (..), BeamableSqlite, addRowsInBatches, combined, deleteRows,
                                        selectList, selectOne, selectPage, updateRows)
import Control.Monad.Freer.Extras.Log (LogMsg, logDebug, logError, logWarn)
import Control.Monad.Freer.Extras.Pagination (Page (Page), PageQuery (..))
import Control.Monad.Freer.Reader (Reader, ask)
import Control.Monad.Freer.State (State, get, gets, put)
import Data.ByteString (ByteString)
import Data.FingerTree qualified as FT
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Monoid (Ap (..))
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Word (Word64)
import Database.Beam (Columnar, Identity, SqlSelect, TableEntity, aggregate_, all_, countAll_, delete, filter_, guard_,
                      in_, limit_, not_, nub_, select, val_)
import Database.Beam.Backend.SQL (BeamSqlBackendCanSerialize)
import Database.Beam.Query (HasSqlEqualityCheck, asc_, desc_, exists_, orderBy_, update, (&&.), (<-.), (<.), (==.),
                            (>.))
import Database.Beam.Schema.Tables (zipTables)
import Database.Beam.Sqlite (Sqlite)
import Ledger (Address (..), ChainIndexTxOut (..), Datum, DatumHash (..), TxId (..), TxOut (..), TxOutRef (..), ValidatorHash (..), Validator (..), Slot(..))
import Ledger.Crypto (PubKeyHash(..))
import Ledger.Credential (Credential (PubKeyCredential), StakingCredential (StakingHash))
import Ledger.Value (AssetClass (AssetClass), CurrencySymbol, TokenName, Value, flattenValue, singleton, currencySymbol, tokenName)
import Plutus.ChainIndex.Api (IsUtxoResponse (IsUtxoResponse), TxosResponse (TxosResponse),
                              UtxosResponse (UtxosResponse))
import Plutus.ChainIndex.ChainIndexError (ChainIndexError (..))
import Plutus.ChainIndex.ChainIndexLog (ChainIndexLog (..))
import Plutus.ChainIndex.Compatibility (toCardanoPoint)
import Plutus.ChainIndex.DbSchema
import Plutus.ChainIndex.Effects (ChainIndexControlEffect (..), ChainIndexQueryEffect (..))
import Plutus.ChainIndex.Tx
import Plutus.ChainIndex.TxUtxoBalance qualified as TxUtxoBalance
import Plutus.ChainIndex.Types (ChainSyncBlock (..), Depth (..), Diagnostics (..), Point (..), Tip (..), BlockNumber(..), BlockId(..),
                                TxProcessOption (..), TxUtxoBalance (..), tipAsPoint)
import Plutus.ChainIndex.UtxoState (InsertUtxoSuccess (..), RollbackResult (..), UtxoIndex)
import Plutus.ChainIndex.UtxoState qualified as UtxoState
import Plutus.V1.Ledger.Ada qualified as Ada
import Plutus.V1.Ledger.Api (Credential (PubKeyCredential, ScriptCredential))

import Blockfrost.Freer.Client (BlockfrostError, ClientEffects)
import Blockfrost.Freer.Client qualified as Blockfrost

-- conversions
import Data.Text (Text)
import qualified Data.ByteString.Char8
import qualified Data.Text
import qualified Cardano.Api.Shelley as Api
import qualified PlutusTx
import qualified PlutusTx.Prelude

import Codec.Serialise qualified as CBOR
import Data.ByteString.Base16 qualified as Base16

import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Short qualified as BS

import Control.Monad
import Control.Monad.IO.Class

import qualified Money

import Data.String (fromString)

import qualified Cardano.Address
import Cardano.Address.Style.Shelley (AddressInfo(..), InspectAddress(..))
import qualified Cardano.Address.Style.Shelley

type ChainIndexState = UtxoIndex TxUtxoBalance

getResumePoints :: Member BeamEffect effs => Eff effs [C.ChainPoint]
getResumePoints
    = fmap (mapMaybe (toCardanoPoint . tipAsPoint . fromDbValue . Just))
    . selectList . select . orderBy_ (desc_ . _tipRowSlot) . all_ $ tipRows db

handleQuery ::
    ( Member (State ChainIndexState) effs
    , Member BeamEffect effs
    , Members ClientEffects effs
    , Member (Error ChainIndexError) effs
    , Member (LogMsg ChainIndexLog) effs
    , LastMember IO effs
    ) => ChainIndexQueryEffect
    ~> Eff effs
handleQuery = \case
    DatumFromHash dh            -> getDatumFromHash dh
    ValidatorFromHash hash      -> getScriptFromHash hash
    MintingPolicyFromHash hash  -> getScriptFromHash' hash
    RedeemerFromHash hash       -> getRedeemerFromHash hash
    StakeValidatorFromHash hash -> getScriptFromHash' hash
    TxFromTxId txId             -> getTxFromTxId txId
    TxOutFromRef tor            -> getTxOutFromRef tor
    UtxoSetMembership r -> do
        utxoState <- gets @ChainIndexState UtxoState.utxoState
        case UtxoState.tip utxoState of
            TipAtGenesis -> throwError QueryFailedNoTip
            tp           -> pure (IsUtxoResponse tp (TxUtxoBalance.isUnspentOutput r utxoState))
    UtxoSetAtAddress pageQuery cred -> getUtxoSetAtAddress pageQuery cred
    UtxoSetWithCurrency pageQuery assetClass ->
      getUtxoSetWithCurrency pageQuery assetClass
    TxoSetAtAddress pageQuery cred -> getTxoSetAtAddress pageQuery cred
    TxsFromTxIds txids             -> getTxsFromTxIds txids
    GetTip -> getTip

getTip' :: Member BeamEffect effs => Eff effs Tip
getTip' = fmap fromDbValue . selectOne . select $ limit_ 1 (orderBy_ (desc_ . _tipRowSlot) (all_ (tipRows db)))

getTipB :: (LastMember IO effs, Members ClientEffects effs) => Eff effs Tip
getTipB = bfBlockToTip <$> Blockfrost.getLatestBlock

bfBlockToTip :: Blockfrost.Block -> Tip
bfBlockToTip Blockfrost.Block{Blockfrost._blockHash=bh, Blockfrost._blockSlot=Just (Blockfrost.Slot bs), Blockfrost._blockHeight=Just bheight} = Tip {
    tipSlot = Slot bs
  , tipBlockId = BlockId (either (error . show) id $ Base16.decode $ Data.ByteString.Char8.pack $ Data.Text.unpack $ Blockfrost.unBlockHash bh)
  , tipBlockNo = BlockNumber (fromInteger bheight)
  }
bfBlockToTip _ = TipAtGenesis

getTip :: (LastMember IO effs, Member BeamEffect effs, Members ClientEffects effs) => Eff effs Tip
getTip = comparingResponses getTip' getTipB

comparingResponses
  :: (Eq a, Show a, LastMember IO effs)
  => Eff effs a
  -> Eff effs a
  -> Eff effs a
comparingResponses a b = do
  xa <- a
  xb <- b
  when (xa /= xb) $ liftIO $ do
    putStrLn "Responses differ"
    putStrLn "Response A"
    print xa
    putStrLn "Response B"
    print xb
  pure xa

getDatumFromHash' :: Member BeamEffect effs => DatumHash -> Eff effs (Maybe Datum)
getDatumFromHash' = queryOne . queryKeyValue datumRows _datumRowHash _datumRowDatum

-- Blockfrost.DatumHash "d22d3b69db8edb4ee2df2a375e68f2aaecac34f6b9fa320d7a1ebaa44a706d90"
toBlockfrostDatumHash :: DatumHash -> Blockfrost.DatumHash
toBlockfrostDatumHash = Blockfrost.DatumHash . Data.Text.pack . show

fromBlockfrostDatumHash :: Blockfrost.DatumHash -> ValidatorHash
fromBlockfrostDatumHash = fromString . Data.Text.unpack . Blockfrost.unDatumHash

getDatumFromHashB :: (LastMember IO effs, Members ClientEffects effs) => DatumHash -> Eff effs (Maybe Datum)
getDatumFromHashB dh = do
  eres <- Blockfrost.tryError $ Blockfrost.getScriptDatum $ toBlockfrostDatumHash dh
  case eres of
    Left Blockfrost.BlockfrostNotFound -> pure Nothing
    Left e -> throwError e
    Right (Blockfrost.ScriptDatum sdt) -> do
      case Api.scriptDataFromJson Api.ScriptDataJsonDetailedSchema sdt of
            Right dec -> do
              case PlutusTx.fromBuiltinData $ PlutusTx.dataToBuiltinData $ Api.toPlutusData dec of
                Just x -> return $ pure x
                Nothing -> return $ Nothing
            Left err -> error $ "Datum transcoding error:" ++ show err

getDatumFromHash :: (LastMember IO effs, Member BeamEffect effs, Members ClientEffects effs) => DatumHash -> Eff effs (Maybe Datum)
getDatumFromHash =
 liftM2 comparingResponses getDatumFromHash' getDatumFromHashB

getTxFromTxId :: Member BeamEffect effs => TxId -> Eff effs (Maybe ChainIndexTx)
getTxFromTxId = queryOne . queryKeyValue txRows _txRowTxId _txRowTx

-- Blockfrost.ScriptHash "67f33146617a5e61936081db3b2117cbf59bd2123748f58ac9678656"
toBlockfrostScriptHash :: ValidatorHash -> Blockfrost.ScriptHash
toBlockfrostScriptHash = Blockfrost.ScriptHash . Data.Text.pack . show

fromBlockfrostScriptHash :: Blockfrost.ScriptHash -> ValidatorHash
fromBlockfrostScriptHash = fromString . Data.Text.unpack . Blockfrost.unScriptHash

getScriptFromHashB :: (LastMember IO effs, Members ClientEffects effs) => ValidatorHash -> Eff effs (Maybe Validator)
getScriptFromHashB vh = do
  let hash = toBlockfrostScriptHash vh
  eres <- Blockfrost.tryError $ Blockfrost.getScript hash
  case eres of
    Left Blockfrost.BlockfrostNotFound -> pure Nothing
    Left e -> throwError e
    --Right (Blockfrost.Script{..} ) -> do
    Right (_scr) -> do
      -- TODO: assert that it is plututs type script
      Blockfrost.ScriptCBOR b16cbor <- Blockfrost.getScriptCBOR hash
      case Base16.decode . Data.ByteString.Char8.pack . Data.Text.unpack <$> b16cbor of
        Nothing -> undefined
        Just x -> case x of
          Left e -> error $ "Can't Base16 decode script CBOR " ++ show e
          Right dec -> do
            let Right (Api.PlutusScriptSerialised sbs) = Api.deserialiseFromCBOR (Api.AsPlutusScript Api.AsPlutusScriptV1) dec
            let n = CBOR.deserialiseOrFail $ BL.fromStrict $ BS.fromShort sbs
            case n of
              Left ex -> error $ "Can't CBOR deserialise script " ++ show ex
              Right v -> return $ pure $ Validator v


getScriptFromHash' ::
    ( Member BeamEffect effs
    , HasDbType i
    , DbType i ~ ByteString
    , HasDbType o
    , DbType o ~ ByteString
    ) => i
    -> Eff effs (Maybe o)
getScriptFromHash' = queryOne . queryKeyValue scriptRows _scriptRowHash _scriptRowScript

getScriptFromHash :: (LastMember IO effs, Member BeamEffect effs, Members ClientEffects effs) => ValidatorHash -> Eff effs (Maybe Validator)
getScriptFromHash =
 liftM2 comparingResponses getScriptFromHash' getScriptFromHashB

getRedeemerFromHash ::
    ( Member BeamEffect effs
    , HasDbType i
    , DbType i ~ ByteString
    , HasDbType o
    , DbType o ~ ByteString
    ) => i
    -> Eff effs (Maybe o)
getRedeemerFromHash = queryOne . queryKeyValue redeemerRows _redeemerRowHash _redeemerRowRedeemer

queryKeyValue ::
    ( HasDbType key
    , HasSqlEqualityCheck Sqlite (DbType key)
    , BeamSqlBackendCanSerialize Sqlite (DbType key)
    ) => (forall f. Db f -> f (TableEntity table))
    -> (forall f. table f -> Columnar f (DbType key))
    -> (forall f. table f -> Columnar f value)
    -> key
    -> SqlSelect Sqlite value
queryKeyValue table getKey getValue (toDbValue -> key) =
    select $ getValue <$> filter_ (\row -> getKey row ==. val_ key) (all_ (table db))

queryOne ::
    ( Member BeamEffect effs
    , HasDbType o
    ) => SqlSelect Sqlite (DbType o)
    -> Eff effs (Maybe o)
queryOne = fmap (fmap fromDbValue) . selectOne

queryList ::
    ( Member BeamEffect effs
    , HasDbType o
    ) => SqlSelect Sqlite (DbType o)
    -> Eff effs [o]
queryList = fmap (fmap fromDbValue) . selectList

-- | Get the 'ChainIndexTxOut' for a 'TxOutRef'.
getTxOutFromRef' ::
  forall effs.
  ( Member BeamEffect effs
  , Member (LogMsg ChainIndexLog) effs
  , Members ClientEffects effs
  , LastMember IO effs
  )
  => TxOutRef
  -> Eff effs (Maybe ChainIndexTxOut)
getTxOutFromRef' ref@TxOutRef{txOutRefId, txOutRefIdx} = do
  mTx <- getTxFromTxId txOutRefId
  -- Find the output in the tx matching the output ref
  case mTx ^? _Just . citxOutputs . _ValidTx . ix (fromIntegral txOutRefIdx) of
    Nothing -> logWarn (TxOutNotFound ref) >> pure Nothing
    Just txout -> do
      -- The output might come from a public key address or a script address.
      -- We need to handle them differently.
      case addressCredential $ txOutAddress txout of
        PubKeyCredential _ ->
          pure $ Just $ PublicKeyChainIndexTxOut (txOutAddress txout) (txOutValue txout)
        ScriptCredential vh -> do
          case txOutDatumHash txout of
            Nothing -> do
              -- If the txout comes from a script address, the Datum should not be Nothing
              logWarn $ NoDatumScriptAddr txout
              pure Nothing
            Just dh -> do
                v <- maybe (Left vh) Right <$> getScriptFromHash vh
                d <- maybe (Left dh) Right <$> getDatumFromHash dh
                pure $ Just $ ScriptChainIndexTxOut (txOutAddress txout) v d (txOutValue txout)

-- | Get the 'ChainIndexTxOut' for a 'TxOutRef'.
getTxOutFromRefB ::
  forall effs.
  ( Member (LogMsg ChainIndexLog) effs
  , Members ClientEffects effs
  , LastMember IO effs
  )
  => TxOutRef
  -> Eff effs (Maybe ChainIndexTxOut)
getTxOutFromRefB ref@TxOutRef{txOutRefId, txOutRefIdx} = do
  let hash = Blockfrost.TxHash $ Data.Text.pack $ show txOutRefId
  eres <- Blockfrost.tryError $ Blockfrost.getTxUtxos hash
  case eres of
    Left Blockfrost.BlockfrostNotFound -> logWarn (TxOutNotFound ref) >> pure Nothing
    Left e -> throwError e
    Right res -> do
      let out = filter ((==txOutRefIdx) . view Blockfrost.outputIndex) (res ^. Blockfrost.outputs)
      -- liftIO $ print out
      case out of
        [txo] -> do
          let addr = txo ^. Blockfrost.address
          let inspectAddr = Cardano.Address.Style.Shelley.eitherInspectAddress Nothing . maybe (error "fromBech32 failed") id . Cardano.Address.fromBech32 . Blockfrost.unAddress $ addr
          let txoValue = mconcat $ map amountToValue $ txo ^. Blockfrost.amount

          case inspectAddr of
            Right (InspectAddressShelley (AddressInfo {
                infoSpendingKeyHash = Just spendingKeyHash
              , infoStakeKeyHash = Just stakeKeyHash })) -> do
                  let plutusAddr =
                        Address {
                          addressCredential = PubKeyCredential $ PubKeyHash $ PlutusTx.Prelude.toBuiltin spendingKeyHash
                        , addressStakingCredential = Just (StakingHash (PubKeyCredential $ PubKeyHash $ PlutusTx.Prelude.toBuiltin stakeKeyHash))
                        }

                  pure $ Just $ PublicKeyChainIndexTxOut plutusAddr txoValue

            Right (InspectAddressShelley (AddressInfo {
                infoScriptHash = Just scriptHash })) -> do

                  let vh = ValidatorHash $ PlutusTx.Prelude.toBuiltin scriptHash
                      dh = fromString $ Data.Text.unpack
                              $ maybe (error "Script credential but data hash is nothing") id
                              $ txo ^. Blockfrost.dataHash

                  -- TODO: B suffixed because comparing forces Beam eff
                  v <- maybe (Left vh) Right <$> getScriptFromHashB vh
                  d <- maybe (Left dh) Right <$> getDatumFromHashB dh

                  let plutusAddr = Address {
                        addressCredential = ScriptCredential vh
                      , addressStakingCredential = Nothing
                      }

                  pure $ Just $ ScriptChainIndexTxOut plutusAddr v d txoValue

            Left e -> error $ "Error inspecting address " ++ show e
            o -> error $ "Unexpected inspect address result " ++ show o

        _ -> pure Nothing

getTxOutFromRef
  :: (LastMember IO effs
    , Member (LogMsg ChainIndexLog) effs
    , Member BeamEffect effs
    , Members ClientEffects effs)
  => TxOutRef
  -> Eff effs (Maybe ChainIndexTxOut)
getTxOutFromRef =
 liftM2 comparingResponses getTxOutFromRef' getTxOutFromRefB

amountToValue :: Blockfrost.Amount -> Value
amountToValue (Blockfrost.AdaAmount disc) = Ada.lovelaceValueOf $ Money.someDiscreteAmount $ Money.toSomeDiscrete disc
amountToValue (Blockfrost.AssetAmount someDisc) =
  singleton curSym tok $ Money.someDiscreteAmount someDisc
  where
    (curSym, tok) = decodePolicyToken $ Money.someDiscreteCurrency someDisc

decodePolicyToken :: Text -> (CurrencySymbol, TokenName)
decodePolicyToken concatenated =
  let
    cb = Data.ByteString.Char8.pack $ Data.Text.unpack concatenated
    curSym = currencySymbol
      $ either (error . show) id
      $ Base16.decode $ Data.ByteString.Char8.take 56 cb
    tok = tokenName
      $ either (error . show) id
      $ Base16.decode $ Data.ByteString.Char8.drop 56 cb
  in (curSym, tok)

getUtxoSetAtAddress
  :: forall effs.
    ( Member (State ChainIndexState) effs
    , Member BeamEffect effs
    , Member (LogMsg ChainIndexLog) effs
    )
  => PageQuery TxOutRef
  -> Credential
  -> Eff effs UtxosResponse
getUtxoSetAtAddress pageQuery (toDbValue -> cred) = do
  utxoState <- gets @ChainIndexState UtxoState.utxoState

  case UtxoState.tip utxoState of
      TipAtGenesis -> do
          logWarn TipIsGenesis
          pure (UtxosResponse TipAtGenesis (Page pageQuery Nothing []))
      tp           -> do
          let query =
                fmap _addressRowOutRef
                  $ filter_ (\row ->
                      (_addressRowCred row ==. val_ cred)
                      &&. exists_ (filter_ (\utxo -> _addressRowOutRef row ==. _unspentOutputRowOutRef utxo) (all_ (unspentOutputRows db)))
                      &&. not_ (exists_ (filter_ (\utxi -> _addressRowOutRef row ==. _unmatchedInputRowOutRef utxi) (all_ (unmatchedInputRows db))))
                      )
                  $ all_ (addressRows db)

          outRefs <- selectPage (fmap toDbValue pageQuery) query
          let page = fmap fromDbValue outRefs

          pure (UtxosResponse tp page)

getUtxoSetWithCurrency
  :: forall effs.
    ( Member (State ChainIndexState) effs
    , Member BeamEffect effs
    , Member (LogMsg ChainIndexLog) effs
    )
  => PageQuery TxOutRef
  -> AssetClass
  -> Eff effs UtxosResponse
getUtxoSetWithCurrency pageQuery (toDbValue -> assetClass) = do
  utxoState <- gets @ChainIndexState UtxoState.utxoState

  case UtxoState.tip utxoState of
      TipAtGenesis -> do
          logWarn TipIsGenesis
          pure (UtxosResponse TipAtGenesis (Page pageQuery Nothing []))
      tp           -> do
          let query =
                fmap _assetClassRowOutRef
                  $ filter_ (\row -> _assetClassRowAssetClass row ==. val_ assetClass)
                  $ do
                    utxo <- all_ (unspentOutputRows db)
                    a <- all_ (assetClassRows db)
                    guard_ (_assetClassRowOutRef a ==. _unspentOutputRowOutRef utxo)
                    pure a

          outRefs <- selectPage (fmap toDbValue pageQuery) query
          let page = fmap fromDbValue outRefs

          pure (UtxosResponse tp page)

getTxsFromTxIds
  :: forall effs.
    ( Member BeamEffect effs
    )
  => [TxId]
  -> Eff effs [ChainIndexTx]
getTxsFromTxIds txIds =
  do
    let
      txIds' = toDbValue <$> txIds
      query =
        fmap _txRowTx
          $ filter_ (\row -> _txRowTxId row `in_` fmap val_ txIds')
          $ all_ (txRows db)
    txs <- selectList $ select query
    pure $ fmap fromDbValue txs

getTxoSetAtAddress
  :: forall effs.
    ( Member (State ChainIndexState) effs
    , Member BeamEffect effs
    , Member (LogMsg ChainIndexLog) effs
    )
  => PageQuery TxOutRef
  -> Credential
  -> Eff effs TxosResponse
getTxoSetAtAddress pageQuery (toDbValue -> cred) = do
  utxoState <- gets @ChainIndexState UtxoState.utxoState
  case UtxoState.tip utxoState of
      TipAtGenesis -> do
          logWarn TipIsGenesis
          pure (TxosResponse (Page pageQuery Nothing []))
      _           -> do
          let query =
                fmap _addressRowOutRef
                  $ filter_ (\row -> _addressRowCred row ==. val_ cred)
                  $ all_ (addressRows db)
          txOutRefs' <- selectPage (fmap toDbValue pageQuery) query
          let page = fmap fromDbValue txOutRefs'
          pure $ TxosResponse page

handleControl ::
    forall effs.
    ( Member (State ChainIndexState) effs
    , Member (Reader Depth) effs
    , Member BeamEffect effs
    , Member (Error ChainIndexError) effs
    , Member (LogMsg ChainIndexLog) effs
    )
    => ChainIndexControlEffect
    ~> Eff effs
handleControl = \case
    AppendBlock (Block tip_ transactions) -> do
        oldIndex <- get @ChainIndexState
        let newUtxoState = TxUtxoBalance.fromBlock tip_ (map fst transactions)
        case UtxoState.insert newUtxoState oldIndex of
            Left err -> do
                let reason = InsertionFailed err
                logError $ Err reason
                throwError reason
            Right InsertUtxoSuccess{newIndex, insertPosition} -> do
                depth <- ask @Depth
                case UtxoState.reduceBlockCount depth newIndex of
                  UtxoState.BlockCountNotReduced -> put newIndex
                  lbcResult -> do
                    put $ UtxoState.reducedIndex lbcResult
                    reduceOldUtxoDb $ UtxoState._usTip $ UtxoState.combinedState lbcResult
                insert $ foldMap (\(tx, opt) -> if tpoStoreTx opt then fromTx tx else mempty) transactions
                insertUtxoDb newUtxoState
                logDebug $ InsertionSuccess tip_ insertPosition
    Rollback tip_ -> do
        oldIndex <- get @ChainIndexState
        case TxUtxoBalance.rollback tip_ oldIndex of
            Left err -> do
                let reason = RollbackFailed err
                logError $ Err reason
                throwError reason
            Right RollbackResult{newTip, rolledBackIndex} -> do
                put rolledBackIndex
                rollbackUtxoDb $ tipAsPoint newTip
                logDebug $ RollbackSuccess newTip
    ResumeSync tip_ -> do
        rollbackUtxoDb tip_
        newState <- restoreStateFromDb
        put newState
    CollectGarbage -> do
        -- Rebuild the index using only transactions that still have at
        -- least one output in the UTXO set
        utxos <- gets $
            Set.toList
            . Set.map txOutRefId
            . TxUtxoBalance.unspentOutputs
            . UtxoState.utxoState
        insertRows <- foldMap fromTx . catMaybes <$> mapM getTxFromTxId utxos
        combined $
            [ DeleteRows $ truncateTable (datumRows db)
            , DeleteRows $ truncateTable (scriptRows db)
            , DeleteRows $ truncateTable (redeemerRows db)
            , DeleteRows $ truncateTable (txRows db)
            , DeleteRows $ truncateTable (addressRows db)
            , DeleteRows $ truncateTable (assetClassRows db)
            ] ++ getConst (zipTables Proxy (\tbl (InsertRows rows) -> Const [AddRowsInBatches batchSize tbl rows]) db insertRows)
        where
            truncateTable table = delete table (const (val_ True))
    GetDiagnostics -> diagnostics


-- Use a batch size of 400 so that we don't hit the sql too-many-variables
-- limit.
batchSize :: Int
batchSize = 400

insertUtxoDb ::
    ( Member BeamEffect effs
    , Member (Error ChainIndexError) effs
    )
    => UtxoState.UtxoState TxUtxoBalance
    -> Eff effs ()
insertUtxoDb (UtxoState.UtxoState _ TipAtGenesis) = throwError $ InsertionFailed UtxoState.InsertUtxoNoTip
insertUtxoDb (UtxoState.UtxoState (TxUtxoBalance outputs inputs) tip)
    = insert $ mempty
        { tipRows = InsertRows $ catMaybes [toDbValue tip]
        , unspentOutputRows = InsertRows $ UnspentOutputRow tipRowId . toDbValue <$> Set.toList outputs
        , unmatchedInputRows = InsertRows $ UnmatchedInputRow tipRowId . toDbValue <$> Set.toList inputs
        }
        where
            tipRowId = TipRowId (toDbValue (tipSlot tip))

reduceOldUtxoDb :: Member BeamEffect effs => Tip -> Eff effs ()
reduceOldUtxoDb TipAtGenesis = pure ()
reduceOldUtxoDb (Tip (toDbValue -> slot) _ _) = do
    -- Delete all the tips before 'slot'
    deleteRows $ delete (tipRows db) (\row -> _tipRowSlot row <. val_ slot)
    -- Assign all the older utxo changes to 'slot'
    updateRows $ update
        (unspentOutputRows db)
        (\row -> _unspentOutputRowTip row <-. TipRowId (val_ slot))
        (\row -> unTipRowId (_unspentOutputRowTip row) <. val_ slot)
    updateRows $ update
        (unmatchedInputRows db)
        (\row -> _unmatchedInputRowTip row <-. TipRowId (val_ slot))
        (\row -> unTipRowId (_unmatchedInputRowTip row) <. val_ slot)
    -- Among these older changes, delete the matching input/output pairs
    -- We're deleting only the outputs here, the matching input is deleted by a trigger (See Main.hs)
    deleteRows $ delete
        (unspentOutputRows db)
        (\output -> unTipRowId (_unspentOutputRowTip output) ==. val_ slot &&.
            exists_ (filter_
                (\input ->
                    (unTipRowId (_unmatchedInputRowTip input) ==. val_ slot) &&.
                    (_unspentOutputRowOutRef output ==. _unmatchedInputRowOutRef input))
                (all_ (unmatchedInputRows db))))

rollbackUtxoDb :: Member BeamEffect effs => Point -> Eff effs ()
rollbackUtxoDb PointAtGenesis = deleteRows $ delete (tipRows db) (const (val_ True))
rollbackUtxoDb (Point (toDbValue -> slot) _) = do
    deleteRows $ delete (tipRows db) (\row -> _tipRowSlot row >. val_ slot)
    deleteRows $ delete (unspentOutputRows db) (\row -> unTipRowId (_unspentOutputRowTip row) >. val_ slot)
    deleteRows $ delete (unmatchedInputRows db) (\row -> unTipRowId (_unmatchedInputRowTip row) >. val_ slot)

restoreStateFromDb :: Member BeamEffect effs => Eff effs ChainIndexState
restoreStateFromDb = do
    uo <- selectList . select $ all_ (unspentOutputRows db)
    ui <- selectList . select $ all_ (unmatchedInputRows db)
    let balances = Map.fromListWith (<>) $ fmap outputToTxUtxoBalance uo ++ fmap inputToTxUtxoBalance ui
    tips <- selectList . select
        . orderBy_ (asc_ . _tipRowSlot)
        $ all_ (tipRows db)
    pure $ FT.fromList . fmap (toUtxoState balances) $ tips
    where
        outputToTxUtxoBalance :: UnspentOutputRow -> (Word64, TxUtxoBalance)
        outputToTxUtxoBalance (UnspentOutputRow (TipRowId slot) outRef)
            = (slot, TxUtxoBalance (Set.singleton (fromDbValue outRef)) mempty)
        inputToTxUtxoBalance :: UnmatchedInputRow -> (Word64, TxUtxoBalance)
        inputToTxUtxoBalance (UnmatchedInputRow (TipRowId slot) outRef)
            = (slot, TxUtxoBalance mempty (Set.singleton (fromDbValue outRef)))
        toUtxoState :: Map.Map Word64 TxUtxoBalance -> TipRow -> UtxoState.UtxoState TxUtxoBalance
        toUtxoState balances tip@(TipRow slot _ _)
            = UtxoState.UtxoState (Map.findWithDefault mempty slot balances) (fromDbValue (Just tip))

data InsertRows te where
    InsertRows :: BeamableSqlite t => [t Identity] -> InsertRows (TableEntity t)

instance Semigroup (InsertRows te) where
    InsertRows l <> InsertRows r = InsertRows (l <> r)
instance BeamableSqlite t => Monoid (InsertRows (TableEntity t)) where
    mempty = InsertRows []

insert :: Member BeamEffect effs => Db InsertRows -> Eff effs ()
insert = getAp . getConst . zipTables Proxy (\tbl (InsertRows rows) -> Const $ Ap $ addRowsInBatches batchSize tbl rows) db

fromTx :: ChainIndexTx -> Db InsertRows
fromTx tx = mempty
    { datumRows = fromMap citxData
    , scriptRows = fromMap citxScripts
    , redeemerRows = fromMap citxRedeemers
    , txRows = InsertRows [toDbValue (_citxTxId tx, tx)]
    , addressRows = fromPairs (fmap credential . txOutsWithRef)
    , assetClassRows = fromPairs (concatMap assetClasses . txOutsWithRef)
    }
    where
        credential :: (TxOut, TxOutRef) -> (Credential, TxOutRef)
        credential (TxOut{txOutAddress=Address{addressCredential}}, ref) =
          (addressCredential, ref)
        assetClasses :: (TxOut, TxOutRef) -> [(AssetClass, TxOutRef)]
        assetClasses (TxOut{txOutValue}, ref) =
          fmap (\(c, t, _) -> (AssetClass (c, t), ref))
               -- We don't store the 'AssetClass' when it is the Ada currency.
               $ filter (\(c, t, _) -> not $ Ada.adaSymbol == c && Ada.adaToken == t)
               $ flattenValue txOutValue
        fromMap
            :: (BeamableSqlite t, HasDbType (k, v), DbType (k, v) ~ t Identity)
            => Lens' ChainIndexTx (Map.Map k v)
            -> InsertRows (TableEntity t)
        fromMap l = fromPairs (Map.toList . view l)
        fromPairs
            :: (BeamableSqlite t, HasDbType (k, v), DbType (k, v) ~ t Identity)
            => (ChainIndexTx -> [(k, v)])
            -> InsertRows (TableEntity t)
        fromPairs l = InsertRows . fmap toDbValue . l $ tx


diagnostics ::
    ( Member BeamEffect effs
    , Member (State ChainIndexState) effs
    ) => Eff effs Diagnostics
diagnostics = do
    numTransactions <- selectOne . select $ aggregate_ (const countAll_) (all_ (txRows db))
    txIds <- queryList . select $ _txRowTxId <$> limit_ 10 (all_ (txRows db))
    numScripts <- selectOne . select $ aggregate_ (const countAll_) (all_ (scriptRows db))
    numAddresses <- selectOne . select $ aggregate_ (const countAll_) $ nub_ $ _addressRowCred <$> all_ (addressRows db)
    numAssetClasses <- selectOne . select $ aggregate_ (const countAll_) $ nub_ $ _assetClassRowAssetClass <$> all_ (assetClassRows db)
    TxUtxoBalance outputs inputs <- UtxoState._usTxUtxoData . UtxoState.utxoState <$> get @ChainIndexState

    pure $ Diagnostics
        { numTransactions    = fromMaybe (-1) numTransactions
        , numScripts         = fromMaybe (-1) numScripts
        , numAddresses       = fromMaybe (-1) numAddresses
        , numAssetClasses    = fromMaybe (-1) numAssetClasses
        , numUnspentOutputs  = length outputs
        , numUnmatchedInputs = length inputs
        , someTransactions   = txIds
        }
