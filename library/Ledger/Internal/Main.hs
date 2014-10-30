module Ledger.Internal.Main
  ( State
  , loadConfig
  , loadState
  ) where

import           Ledger.Models           (Entry)

import           Data.Acid               (AcidState)
import           Data.Acid.Memory        (openMemoryState)
import           Data.Configurator       (Worth (Required), load)
import           Data.Configurator.Types (Config)
import           System.Environment      (getArgs)

type State = AcidState [Entry]

loadConfig :: IO Config
loadConfig = do
  arguments <- getArgs
  let paths = map Required arguments
  load paths

loadState :: IO State
loadState = do
  let entries = [] :: [Entry]
  openMemoryState entries
