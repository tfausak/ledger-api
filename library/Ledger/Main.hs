{-# LANGUAGE OverloadedStrings #-}

module Ledger.Main
  ( main
  ) where

import           Ledger.Application       (application)
import           Ledger.Internal.Main     (loadConfig)
import           Ledger.Models            (Entry)

import           Data.Acid.Memory         (openMemoryState)
import           Data.Configurator        (lookupDefault)
import           Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  config <- loadConfig
  port <- lookupDefault 8080 config "port"
  let entries = [] :: [Entry]
  state <- openMemoryState entries
  run port (application state)
