{-# LANGUAGE OverloadedStrings #-}

module Ledger.Utilities where

import           Paths_ledger       (getDataFileName)

import           Data.Aeson         (ToJSON, encode)
import           Data.Map           (fromList, insert, toList)
import           Network.HTTP.Types (ResponseHeaders, Status, hContentType)
import           Network.Wai        (Response, responseLBS)
import           System.Environment (lookupEnv)
import           System.FilePath    ((</>))

getFileName :: FilePath -> IO FilePath
getFileName path = do
  maybeDirectory <- lookupEnv "OPENSHIFT_DATA_DIR"
  case maybeDirectory of
    Just directory -> return (directory </> path)
    Nothing -> getDataFileName path

json :: (ToJSON a) => Status -> ResponseHeaders -> a -> Response
json status headers value = responseLBS
  status
  (toList (insert hContentType "application/json" (fromList headers)))
  (encode value)
