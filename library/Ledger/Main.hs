module Ledger.Main where

import Ledger.Application (application, defaultState, middleware)
import Ledger.Config (getConfig)
import Ledger.Settings (getSettings)
import Ledger.State (getState)

import Network.Wai.Handler.Warp (runSettings)

main :: IO ()
main = do
    config <- getConfig
    settings <- getSettings config
    state <- getState config defaultState
    let fullApplication = middleware (application state)
    runSettings settings fullApplication
