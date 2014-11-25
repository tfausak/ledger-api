{-# LANGUAGE OverloadedStrings #-}

module Ledger.Utilities where

import           Paths_ledger         (getDataFileName)

import           Data.Aeson           (ToJSON, encode)
import           Data.ByteString      (ByteString)
import           Data.ByteString.Lazy (readFile)
import           Data.Map             (fromList, insert, toList)
import           Network.HTTP.Types   (ResponseHeaders, Status, hCacheControl,
                                       hContentType, status200)
import           Network.Wai          (Response, responseLBS)
import           Prelude              hiding (readFile)
import           System.Environment   (lookupEnv)
import           System.FilePath      ((</>))

file :: FilePath -> ByteString -> IO Response
file path contentType = do
  name <- getFileName path
  body <- readFile name
  let status = status200
  let headers =
        [ (hCacheControl, "max-age=86400")
        , (hContentType, contentType)
        ]
  let response = responseLBS status headers body
  return response

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
