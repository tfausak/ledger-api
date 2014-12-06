{-# LANGUAGE OverloadedStrings #-}

module Ledger.Settings (getSettings) where

import Control.Monad.State (State, execState, modify)
import Data.Configurator (require)
import Data.Configurator.Types (Config)
import Data.String (fromString)
import Network.Wai.Handler.Warp (HostPreference, Settings, defaultSettings,
                                 setHost, setPort)
import Text.Read (readMaybe)

getSettings :: Config -> IO Settings
getSettings config = do
    host <- getHost config
    maybePort <- getPort config
    let builder = buildSettings host maybePort
    let settings = execState builder defaultSettings
    return settings

getHost :: Config -> IO HostPreference
getHost config = fmap fromString (require config "warp.host")

getPort :: Config -> IO (Maybe Int)
getPort config = fmap readMaybe (require config "warp.port")

buildSettings :: HostPreference -> Maybe Int -> State Settings ()
buildSettings host maybePort = do
    modify (setHost host)
    modify (maybe id setPort maybePort)
