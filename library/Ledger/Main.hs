module Ledger.Main where

import Ledger.Application (application, defaultState, middleware)
import Ledger.Config (getConfig)
import Ledger.Settings (getSettings)
import Ledger.State (getState)

import Network.Wai.Handler.Warp (Settings, getHost, getPort, runSettings)

main :: IO ()
main = do
    config <- getConfig
    settings <- getSettings config
    state <- getState config defaultState
    let fullApplication = middleware (application state)
    putStrLn (message settings)
    runSettings settings fullApplication

message :: Settings -> String
message settings = concat
    [ show (getHost settings)
    , ":"
    , show (getPort settings)
    ]
