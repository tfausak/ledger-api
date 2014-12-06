{-# LANGUAGE OverloadedStrings #-}

module Ledger.State (getState) where

import Data.Acid (AcidState, IsAcidic)
import Data.Acid.Local (openLocalStateFrom)
import Data.Acid.Memory (openMemoryState)
import Data.Acid.Remote (CommChannel, openRemoteState, sharedSecretPerform,
                         skipAuthenticationPerform)
import Data.Configurator (lookup)
import Data.Configurator.Types (Config)
import Data.Maybe (catMaybes)
import Network (PortID (PortNumber))
import Prelude hiding (lookup)

getState :: (IsAcidic a) => Config -> a -> IO (AcidState a)
getState config defaultState = do
    remoteState <- getRemoteState config
    localState <- getLocalState config defaultState
    memoryState <- getMemoryState defaultState
    let state = head (catMaybes [remoteState, localState, Just memoryState])
    return state

getRemoteState :: (IsAcidic a) => Config -> IO (Maybe (AcidState a))
getRemoteState config = do
    maybeHost <- lookup config "acid-state.host"
    maybePort <- lookup config "acid-state.port"
    authentication <- getRemoteAuthentication config
    case (maybeHost, maybePort) of
        (Just host, Just port) -> do
            let portNumber = PortNumber (fromIntegral (port :: Int))
            state <- openRemoteState authentication host portNumber
            return (Just state)
        _ -> return Nothing

getLocalState :: (IsAcidic a) => Config -> a -> IO (Maybe (AcidState a))
getLocalState config defaultState = do
    maybeDirectory <- lookup config "acid-state.directory"
    case maybeDirectory of
        Just directory -> do
            state <- openLocalStateFrom directory defaultState
            return (Just state)
        Nothing -> return Nothing

getMemoryState :: (IsAcidic a) => a -> IO (AcidState a)
getMemoryState = openMemoryState

getRemoteAuthentication :: Config -> IO (CommChannel -> IO ())
getRemoteAuthentication config = do
    maybeSecret <- lookup config "acid-state.secret"
    let authentication = case maybeSecret of
            Just secret -> sharedSecretPerform secret
            Nothing -> skipAuthenticationPerform
    return authentication
