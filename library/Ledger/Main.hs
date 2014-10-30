{-# LANGUAGE OverloadedStrings #-}

module Ledger.Main
  ( main
  ) where

import           Ledger.Application       (application)
import           Ledger.Internal.Main     (loadConfig)

import           Data.Configurator        (lookupDefault)
import           Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  config <- loadConfig
  port <- lookupDefault 8080 config "port"
  run port application
