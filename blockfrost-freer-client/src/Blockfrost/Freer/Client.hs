-- | Blockfrost freer client
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Blockfrost.Freer.Client (
    module Blockfrost.Client

  -- Eff
  , ClientEffects
  , defaultBlockfrostHandler
  , handleBlockfrostClientEffects
  , runBlockfrost
  , tryError
  )
  where

import Blockfrost.Client hiding (runBlockfrost, tryError)

import Control.Monad.IO.Class
import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Control.Monad.Freer.Error

import Servant.Client

instance
  ( MonadIO m
  , LastMember m effs
  , Member (Reader ClientConfig) effs
  , Member (Error BlockfrostError) effs
  )
  => MonadBlockfrost (Eff effs) where

  liftBlockfrostClient act = getConf >>= \(env, _prj) ->
    (liftIO $ runClientM act env) >>= either (throwError . fromServantClientError) pure

  getConf = ask

type ClientEffects' effs =
    Reader ClientConfig
 ': Error BlockfrostError
 ': effs

type ClientEffects = ClientEffects' '[]

handleBlockfrostClientEffects ::
  forall m effs a .
  ( LastMember m effs
  , MonadIO m
  )
  => ClientConfig
  -> Eff (ClientEffects' effs) a
  -> Eff effs (Either BlockfrostError a)
handleBlockfrostClientEffects cfg act = do
      runError @BlockfrostError
    . runReader @ClientConfig cfg
    $ act

defaultBlockfrostHandler
  :: forall effs a
  . (LastMember IO effs)
  => IO (Eff (ClientEffects' effs) a -> Eff effs (Either BlockfrostError a))
defaultBlockfrostHandler = do
  cfg <- newClientConfig
  pure $ handleBlockfrostClientEffects cfg

runBlockfrost
  :: Project
  -> Eff (ClientEffects' '[IO]) a
  -> IO (Either BlockfrostError a)
runBlockfrost prj act = do
  env <- newEnvByProject prj
  runM . handleBlockfrostClientEffects (env, prj) $ act

-- Catch error effect @Error@ and return
-- either result or error type.
tryError
  :: Member (Error e) effs
  => Eff effs a
  -> Eff effs (Either e a)
tryError act = (Right <$> act) `catchError` (pure . Left)

_example :: IO ()
_example = do
  bfHandler <- defaultBlockfrostHandler
  x <- runM
    . bfHandler
    $ getLatestBlock
  print x
