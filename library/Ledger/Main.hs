module Ledger.Main where

import           Ledger.Application       (application)
import           Ledger.Loader            (loadConfig, loadSettings, loadState)

import           Network.Wai.Handler.Warp (runSettings)

main :: IO ()
main = do
  config <- loadConfig
  settings <- loadSettings config
  state <- loadState config
  runSettings settings (application config state)
