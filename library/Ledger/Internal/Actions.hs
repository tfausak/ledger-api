{-# LANGUAGE OverloadedStrings #-}

module Ledger.Internal.Actions
  ( json
  ) where

import           Data.Aeson         (ToJSON, encode)
import           Data.Map           (fromList, insert, toList)
import           Network.HTTP.Types (ResponseHeaders, Status, hContentType)
import           Network.Wai        (Response, responseLBS)

json :: (ToJSON a) => Status -> ResponseHeaders -> a -> Response
json status headers value = responseLBS
  status
  (toList (insert hContentType "application/json" (fromList headers)))
  (encode value)
