module Ledger.Config (getConfig) where

import Paths_ledger_api (getDataFileName)

import Data.Configurator (Worth (Optional, Required), load)
import Data.Configurator.Types (Config)
import System.Environment (getArgs, lookupEnv)
import System.FilePath ((</>))

getConfig :: IO Config
getConfig = do
    path <- getFilePath "ledger-api.cfg"
    paths <- getArgs
    let worths = Required path : map Optional paths
    load worths

getFilePath :: FilePath -> IO FilePath
getFilePath path = do
    maybeDirectory <- lookupEnv "OPENSHIFT_DATA_DIR"
    case maybeDirectory of
        Just directory -> return (directory </> path)
        Nothing -> getDataFileName path
