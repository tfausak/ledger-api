{-# LANGUAGE OverloadedStrings #-}

module Ledger.Internal.Actions
  ( json
  ) where

import           Data.Aeson         (ToJSON, encode)
import           Data.Map           (alter, fromList, toList)
import           Network.HTTP.Types (ResponseHeaders, Status, hContentType)
import           Network.Wai        (Response, responseLBS)

json :: (ToJSON a) => Status -> ResponseHeaders -> a -> Response
json status headers value = responseLBS
  status
  (toList (alter (const (Just "application/json")) hContentType (fromList headers)))
  (encode value)
