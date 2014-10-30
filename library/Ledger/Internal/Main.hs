module Ledger.Internal.Main
  ( loadConfig
  ) where

import           Data.Configurator       (Worth (Required), load)
import           Data.Configurator.Types (Config)
import           System.Environment      (getArgs)

loadConfig :: IO Config
loadConfig = do
  arguments <- getArgs
  let paths = map Required arguments
  load paths
