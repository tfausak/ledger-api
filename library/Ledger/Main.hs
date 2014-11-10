{-# LANGUAGE OverloadedStrings #-}

module Ledger.Main
  ( main
  , loadConfig
  , loadSettings
  , loadState
  , loadRemoteState
  , loadLocalState
  , loadMemoryState
  ) where

import           Ledger.Application       (application)
import           Ledger.Models.Entry      (Entries)
import           Ledger.State             (State)

import           Data.Acid.Local          (openLocalStateFrom)
import           Data.Acid.Memory         (openMemoryState)
import           Data.Acid.Remote         (openRemoteState, sharedSecretPerform,
                                           skipAuthenticationPerform)
import           Data.Configurator        (Worth (Required), load, lookup)
import           Data.Configurator.Types  (Config)
import           Data.Map                 (fromList)
import           Network                  (PortID (PortNumber))
import           Network.Wai.Handler.Warp (Settings, defaultSettings,
                                           runSettings, setPort)
import           Prelude                  hiding (lookup)
import           System.Environment       (getArgs, getEnv)
import           System.FilePath          ((</>))

main :: IO ()
main = do
  config <- loadConfig
  settings <- loadSettings
  state <- loadState config
  runSettings settings (application config state)

loadConfig :: IO Config
loadConfig = do
  directory <- getEnv "OPENSHIFT_REPO_DIR"
  let name = directory </> "data" </> "ledger.cfg"
  let paths = [Required name]
  load paths

loadSettings :: IO Settings
loadSettings = do
  [_ip, port] <- getArgs
  return (setPort (read port) defaultSettings)

loadState :: Config -> IO State
loadState config = do
  maybeRemoteState <- loadRemoteState config
  case maybeRemoteState of
    Just state -> return state
    Nothing -> do
      maybeLocalState <- loadLocalState config
      case maybeLocalState of
        Just state -> return state
        Nothing -> loadMemoryState

loadRemoteState :: Config -> IO (Maybe State)
loadRemoteState config = do
  maybeHost <- lookup config "acid-state.host"
  case maybeHost of
    Nothing -> return Nothing
    Just host -> do
      maybePort <- lookup config "acid-state.port"
      case maybePort of
        Nothing -> return Nothing
        Just port -> do
          maybeSecret <- lookup config "acid-state.secret"
          let authenticate = case maybeSecret of
                Nothing -> skipAuthenticationPerform
                Just secret -> sharedSecretPerform secret
          let number = fromIntegral (port :: Int)
          state <- openRemoteState authenticate host (PortNumber number)
          return (Just state)

loadLocalState :: Config -> IO (Maybe State)
loadLocalState config = do
  maybeDirectory <- lookup config "acid-state.directory"
  case maybeDirectory of
    Nothing -> return Nothing
    Just directory -> do
      let entries = fromList [] :: Entries
      state <- openLocalStateFrom directory entries
      return (Just state)

loadMemoryState :: IO State
loadMemoryState = do
  let entries = fromList [] :: Entries
  openMemoryState entries
