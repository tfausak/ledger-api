module Ledger.Internal.Main
  ( loadConfig
  , loadState
  ) where

import           Ledger.Models.Entry     (Entry)
import           Ledger.Types            (State)
import           Paths_ledger            (getDataFileName)

import           Data.Acid.Memory        (openMemoryState)
import           Data.Configurator       (Worth (Required), load)
import           Data.Configurator.Types (Config)
import           System.Environment      (getArgs)

loadConfig :: IO Config
loadConfig = do
  name <- getDataFileName "default.cfg"
  names <- getArgs
  let paths = map Required (name : names)
  load paths

loadState :: IO State
loadState = do
  let entries = [] :: [Entry]
  openMemoryState entries
