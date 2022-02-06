{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Main
  where

import Blockfrost.Freer.Client
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Reader
import Control.Monad.Freer.State

-- | Example similar to blockfrost-client main, except using catchError
simpleMain = do
  -- reads token from BLOCKFROST_TOKEN_PATH
  -- environment variable. It expects token
  -- prefixed with Blockfrost environment name
  -- e.g.: testnet-someTokenHash
  prj <- projectFromEnv
  res <- runBlockfrost prj $ do
    latestBlocks <- getLatestBlock
    (ers :: Either BlockfrostError [AccountReward]) <-
      catchError (Right <$> getAccountRewards "gonnaFail")
        (\e -> pure $ Left e)

    -- variant accepting @Paged@ and @SortOrder@ arguments
    -- getAccountRewards' "gonnaFail" (page 10) desc
    pure (latestBlocks, ers)
  print res

-- | Example of using custom effect stack
main = do
  -- reads token from BLOCKFROST_TOKEN_PATH
  -- environment variable. It expects token
  -- prefixed with Blockfrost environment name
  -- e.g.: testnet-someTokenHash
  handleBlockfrostClient <- defaultBlockfrostHandler
  res <-
    runM
      . runState @(Maybe Block) Nothing
      . runReader @Int 42
      . handleBlockfrostClient
      $ do
          latestBlock <- getLatestBlock
          put (Just latestBlock)
          z <- ask @Int
          (ers :: Either BlockfrostError [AccountReward]) <-
            catchError (Right <$> getAccountRewards "gonnaFail")
              (\e -> pure $ Left e)

          pure (latestBlock, ers)
  print res
