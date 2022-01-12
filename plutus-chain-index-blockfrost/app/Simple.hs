module Main where

import qualified Plutus.ChainIndex.Blockfrost
import qualified Plutus.ChainIndex.Lib

main = do
  putStrLn "Running at 3338"
  Plutus.ChainIndex.Lib.withDefaultRunRequirements $ \reqs ->
    Plutus.ChainIndex.Blockfrost.serveChainIndexQueryServer 3338 reqs
