module Main where

import Server

main = do
  putStrLn "Running at 3338"
  Server.serveChainIndexQueryServer 3338
