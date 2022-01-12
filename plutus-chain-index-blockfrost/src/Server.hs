{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Server where

import Plutus.ChainIndex.Api

import Blockfrost.Client
import qualified Blockfrost.Client as Blockfrost
--import Blockfrost.Freer.Client
import Control.Monad.Freer

import Data.Proxy (Proxy (..))

import Servant.API ((:<|>) (..))
import Servant.API.ContentTypes (NoContent (..))
import Servant.Server (Handler, ServerError, ServerT, err404, err500, errBody, hoistServer, serve)

import qualified Network.Wai.Handler.Warp as Warp

--tmp
import Plutus.ChainIndex.Effects (ChainIndexControlEffect, ChainIndexQueryEffect)
import qualified Plutus.ChainIndex.Effects as E

import Control.Monad ((>=>))
import qualified Control.Monad.Except as E
import Control.Monad.Freer (Eff, Member, type (~>))
import Control.Monad.Freer.Error (Error, runError, throwError)
import Control.Monad.Freer.Extras.Modify (raiseEnd)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.ByteString.Lazy as BSL

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Plutus.ChainIndex (RunRequirements, runChainIndexEffects)

import Data.Maybe (fromMaybe)

import Control.Monad.Freer.Reader

import Plutus.ChainIndex.ChainIndexError

import qualified Cardano.Api.Shelley as Api

import Plutus.V1.Ledger.Bytes (bytes)
import qualified Plutus.V1.Ledger.Scripts

import PlutusTx.Builtins (fromBuiltin)
import qualified PlutusTx -- .Builtins

import qualified Data.ByteString.Char8
import qualified Data.Text
import qualified Data.Text.Encoding

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Extras as JSON

serveChainIndexQueryServer ::
    Int -- ^ Port
--    -> RunRequirements
    -> IO ()
serveChainIndexQueryServer port = do -- runReqs = do
    --let server = hoistServer (Proxy @API) (runChainIndexQuery runReqs) serveChainIndex
    let server = 
          pure NoContent
          :<|> fromHashServer -- error "from-hash"
          :<|> error "??"
          -- hoistServer (Proxy @API) (runChainIndexQuery runReqs) serveChainIndex
    Warp.run port (serve (Proxy @FullAPI) (server :<|> swagger))

fromHashServer = 
  datumFromHash
  :<|> error "validatorFromHash"
  :<|> error "mpFromHash"
  :<|> error "stakeValidatorFromHash"
  :<|> error "redeemerFromHash"

-- schema conversion needed
-- see plutus-apps plutus-ledger/src/Ledger/Orphans.hs:108
datumFromHash x@(Plutus.V1.Ledger.Scripts.DatumHash dt) = do
  --error $ show (x, JSON.encodeByteString . bytes $ x)
  --  , dt
  --  , Data.Text.pack $ Data.ByteString.Char8.unpack $ fromBuiltin dt
  --  , Blockfrost.DatumHash $ Data.Text.pack $ Data.ByteString.Char8.unpack $ fromBuiltin dt
  --  )

  res <- query $ getScriptDatum $ Blockfrost.DatumHash $ 
    Data.Text.pack $ show x -- Data.ByteString.Char8.unpack $ fromBuiltin dt
    --Data.Text.Encoding.decodeUtf8 $ fromBuiltin dt
  liftIO $ print res
  case res of
    Right (ScriptDatum sdt) ->
      --error $ show ("got datum from api", sdt)
      case Api.scriptDataFromJson Api.ScriptDataJsonDetailedSchema $ sdt of
        --Right dec -> return $ Plutus.V1.Ledger.Scripts.Datum $ PlutusTx.dataToBuiltinData $ Api.toPlutusData dec
        Right dec -> do
          liftIO $ print dec
          liftIO $ print $ Api.toPlutusData dec

          --return $ PlutusTx.fromBuiltinData $ PlutusTx.dataToBuiltinData $ Api.toPlutusData dec
          case PlutusTx.fromBuiltinData $ PlutusTx.dataToBuiltinData $ Api.toPlutusData dec of
            Just x -> return x
        Left err -> error $ show ("transcoding error:", err)

    Left e -> error $ show ("failed", e)
  --error $ show dt
  where 
    query act = liftIO $ do
      prj <- projectFromEnv
      runBlockfrost prj $ do
        act
    {--

runChainIndexQuery ::
    RunRequirements
    -> Eff '[Error ServerError, ChainIndexQueryEffect] 
--    ~> Handler
--    -> Eff '[Error ServerError, Reader Project, ChainIndexControlEffect] 
--    -> Eff '[Reader Project, Error ServerError] -- , Reader Project]
    ~> Handler
runChainIndexQuery runReq action = do
    result <- liftIO $ runChainIndexEffects' runReq $ runError $ raiseEnd action
    --result <- liftIO $ myRun runReq $ runError $ raiseEnd action
    case result of
        Right (Right a) -> pure a
        Right (Left e) -> E.throwError e
        Left e' ->
            let err = err500 { errBody = BSL.fromStrict $ Text.encodeUtf8 $ Text.pack $ show e' } in
            E.throwError err

myRun
    :: RunRequirements
--    -> Eff '[Reader Project] a
    -> Eff '[ChainIndexQueryEffect, Reader Project] a
    -> IO (Either ChainIndexError a)
myRun = undefined

-- | Run the chain index effects.
runChainIndexEffects'
    :: RunRequirements
    -> Eff '[ChainIndexQueryEffect, ChainIndexControlEffect, Reader Project] a
    -> IO (Either ChainIndexError a)
runChainIndexEffects' runReq action =
    --runLogEffects (trace runReq)
    --id
        handleChainIndexEffects runReq
        $ raiseEnd action

-- | Handle the chain index effects from the set of all effects.
handleChainIndexEffects
    :: (LastMember IO effs) --, Member (LogMsg ChainIndexLog) effs)
    => RunRequirements
    -> Eff (ChainIndexQueryEffect ': ChainIndexControlEffect ': Reader Project ': effs) a
    -> Eff effs (Either ChainIndexError a)
handleChainIndexEffects runReqs action = do 
--RunRequirements{trace, stateMVar, conn, securityParam} action = do
    --state <- liftIO $ takeMVar stateMVar
    result <-
--        runState state
--        $ runReader conn
--        $ runReader (Depth securityParam)
        runReader undefined
        $ runError @ChainIndexError
--        $ flip handleError (throwError . BeamEffectError)
--        $ interpret (handleBeam (convertLog BeamLogItem trace))
--        $ interpret handleControl
--        $ interpret handleQuery
        $ interpret undefined
        $ interpret undefined
        -- Insert the 5 effects needed by the handlers of the 3 chain index e
        $ action
        -- $ raiseMUnderN @[_,_,_,_,_] @[_,_,_] action
    --liftIO $ putMVar stateMVar newState
    pure result
--}

serveChainIndex ::
    forall effs.
    ( Member (Error ServerError) effs
    , Member ChainIndexQueryEffect effs
--    , Member ChainIndexControlEffect effs
--    , Members ClientEffects effs
    )
    => ServerT API (Eff effs)
serveChainIndex = undefined
{--
    pure NoContent
    :<|> serveFromHashApi
    :<|> (E.txOutFromRef >=> handleMaybe)
    :<|> (E.txFromTxId >=> handleMaybe)
    :<|> E.utxoSetMembership
    :<|> (\(UtxoAtAddressRequest pq c) -> E.utxoSetAtAddress (fromMaybe def pq) c)
    :<|> (\(UtxoWithCurrencyRequest pq c) -> E.utxoSetWithCurrency (fromMaybe def pq) c)
    :<|> E.txsFromTxIds
    :<|> (\(TxoAtAddressRequest pq c) -> E.txoSetAtAddress (fromMaybe def pq) c)
    :<|> E.getTip
    :<|> E.collectGarbage *> pure NoContent
    :<|> E.getDiagnostics
--}

serveFromHashApi ::
    forall effs.
    ( Member (Error ServerError) effs
    , Member ChainIndexQueryEffect effs
    )
    => ServerT FromHashAPI (Eff effs)
serveFromHashApi = undefined
{--
    (E.datumFromHash >=> handleMaybe)
    :<|> (E.validatorFromHash >=> handleMaybe)
    :<|> (E.mintingPolicyFromHash >=> handleMaybe)
    :<|> (E.stakeValidatorFromHash >=> handleMaybe)
    :<|> (E.redeemerFromHash >=> handleMaybe)
--}

-- | Return the value of throw a 404 error
handleMaybe :: forall effs. Member (Error ServerError) effs => Maybe ~> Eff effs
handleMaybe = maybe (throwError err404) pure

