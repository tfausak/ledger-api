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

file :: FilePath -> ByteString -> IO Response
file path contentType = do
  name <- getDataFileName path
  body <- readFile name
  let status = status200
  let headers =
        [ (hCacheControl, "max-age=86400")
        , (hContentType, contentType)
        ]
  let response = responseLBS status headers body
  return response

json :: (ToJSON a) => Status -> ResponseHeaders -> a -> Response
json status headers value = responseLBS
  status
  (toList (insert hContentType "application/json" (fromList headers)))
  (encode value)
