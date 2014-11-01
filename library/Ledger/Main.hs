{-# LANGUAGE OverloadedStrings #-}

module Ledger.Main
  ( main
  , loadConfig
  , loadState
  ) where

import           Ledger.Application       (application)
import           Ledger.Models.Entry      (Entries)
import           Ledger.State             (State)
import           Paths_ledger             (getDataFileName)

import           Data.Acid.Memory         (openMemoryState)
import           Data.Configurator        (Worth (Required), load, require)
import           Data.Configurator.Types  (Config)
import           Data.Map                 (fromList)
import           Network.Wai.Handler.Warp (run)
import           System.Environment       (getArgs)

main :: IO ()
main = do
  config <- loadConfig
  port <- require config "port"
  state <- loadState
  run port (application state)

loadConfig :: IO Config
loadConfig = do
  name <- getDataFileName "default.cfg"
  names <- getArgs
  let paths = map Required (name : names)
  load paths

loadState :: IO State
loadState = do
  let entries = fromList [] :: Entries
  openMemoryState entries
