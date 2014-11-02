{-# LANGUAGE OverloadedStrings #-}

module Ledger.Main
  ( main
  , loadConfig
  , loadState
  ) where

import           Ledger.Application       (application)
import           Ledger.Models.Entry      (Entries)
import           Ledger.State             (State)
import           Paths_ledger             (getDataFileName)

import           Data.Acid.Local          (openLocalStateFrom)
import           Data.Acid.Memory         (openMemoryState)
import           Data.Acid.Remote         (openRemoteState, sharedSecretPerform,
                                           skipAuthenticationPerform)
import           Data.Configurator        (Worth (Required), load, lookup,
                                           require)
import           Data.Configurator.Types  (Config)
import           Data.Map                 (fromList)
import           Network                  (PortID (PortNumber, UnixSocket))
import           Network.Wai.Handler.Warp (run)
import           Prelude                  hiding (lookup)
import           System.Environment       (getArgs)

main :: IO ()
main = do
  config <- loadConfig
  port <- require config "warp.port"
  state <- loadState config
  run port (application state)

loadConfig :: IO Config
loadConfig = do
  name <- getDataFileName "default.cfg"
  names <- getArgs
  let paths = map Required (name : names)
  load paths

loadState :: Config -> IO State
loadState config = do
  maybeSecret <- lookup config "acid.secret"
  let authenticate = case maybeSecret of
        Nothing -> skipAuthenticationPerform
        Just secret -> sharedSecretPerform secret

  maybeSocket <- lookup config "acid.socket"
  case maybeSocket of
    Just socket -> openRemoteState authenticate "" (UnixSocket socket)
    Nothing -> do
      maybeHost <- lookup config "acid.host"
      case maybeHost of
        Just host -> do
          port <- require config "acid.port"
          let number = fromIntegral (port :: Int)
          openRemoteState authenticate host (PortNumber number)
        Nothing -> do
          let entries = fromList [] :: Entries
          maybeDirectory <- lookup config "acid.directory"
          case maybeDirectory of
            Just directory -> openLocalStateFrom directory entries
            Nothing -> openMemoryState entries
