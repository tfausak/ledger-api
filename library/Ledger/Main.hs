{-# LANGUAGE OverloadedStrings #-}

module Ledger.Main
  ( main
  ) where

import           Ledger.Application       (application)
import           Ledger.Internal.Main     (loadConfig, loadState)

import           Data.Configurator        (require)
import           Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  config <- loadConfig
  port <- require config "port"
  state <- loadState
  run port (application state)
