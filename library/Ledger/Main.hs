{-# LANGUAGE OverloadedStrings #-}

module Ledger.Main
  ( main
  ) where

import           Ledger.Application       (application)
import           Ledger.Internal.Main     (loadConfig, loadState)

import           Data.Configurator        (lookupDefault)
import           Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  config <- loadConfig
  port <- lookupDefault 8080 config "port"
  state <- loadState
  run port (application state)
