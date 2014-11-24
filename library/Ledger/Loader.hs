{-# LANGUAGE OverloadedStrings #-}

module Ledger.Loader where

import           Ledger.State             (State, defaultState)

import           Data.Acid.Local          (openLocalStateFrom)
import           Data.Acid.Memory         (openMemoryState)
import           Data.Acid.Remote         (CommChannel, openRemoteState,
                                           sharedSecretPerform,
                                           skipAuthenticationPerform)
import           Data.Configurator        (Worth (Required), load, lookup)
import           Data.Configurator.Types  (Config)
import           Data.Maybe               (catMaybes)
import           Data.String              (fromString)
import           Network                  (PortID (PortNumber))
import           Network.Wai.Handler.Warp (Settings, defaultSettings, setHost,
                                           setPort)
import           Prelude                  hiding (lookup)
import           System.Environment       (getArgs, getEnv)
import           System.FilePath          ((</>))

loadConfig :: IO Config
loadConfig = do
  directory <- getEnv "OPENSHIFT_DATA_DIR"
  let name = directory </> "ledger.cfg"
  let paths = [Required name]
  load paths

loadSettings :: IO Settings
loadSettings = do
  [ip, port] <- getArgs
  putStrLn ("http://" ++ ip ++ ":" ++ port)
  return (setHost (fromString ip) (setPort (read port) defaultSettings))

loadState :: Config -> IO State
loadState config = do
  remote <- loadRemoteState config
  local <- loadLocalState config
  memory <- loadMemoryState

  return (head (catMaybes [remote, local, Just memory]))

loadRemoteState :: Config -> IO (Maybe State)
loadRemoteState config = do
  maybeHost <- lookup config "acid-state.host"
  maybePort <- lookup config "acid-state.port"

  case (maybeHost, maybePort) of
    (Just host, Just port) -> do
      authentication <- loadRemoteAuthentication config
      let portNumber = PortNumber (fromIntegral (port :: Int))
      state <- openRemoteState authentication host portNumber
      return (Just state)
    _ -> return Nothing

loadLocalState :: Config -> IO (Maybe State)
loadLocalState config = do
  maybeDirectory <- lookup config "acid-state.directory"
  case maybeDirectory of
    Nothing -> return Nothing
    Just directory -> do
      state <- openLocalStateFrom directory defaultState
      return (Just state)

loadMemoryState :: IO State
loadMemoryState = openMemoryState defaultState

loadRemoteAuthentication :: Config -> IO (CommChannel -> IO ())
loadRemoteAuthentication config = do
  maybeSecret <- lookup config "acid-state.secret"
  let authentication = case maybeSecret of
        Nothing -> skipAuthenticationPerform
        Just secret -> sharedSecretPerform secret
  return authentication
