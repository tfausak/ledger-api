{-# LANGUAGE OverloadedStrings #-}

module Ledger.Internal.Actions
  ( json
  , badRequest
  , notFound
  , notAllowed
  ) where

import           Ledger.Types       (Action)

import           Data.Aeson         (ToJSON, Value (Null), encode)
import           Data.Map           (fromList, insert, toList)
import           Network.HTTP.Types (ResponseHeaders, Status, hContentType,
                                     status400, status404, status405)
import           Network.Wai        (Response, responseLBS)

json :: (ToJSON a) => Status -> ResponseHeaders -> a -> Response
json status headers value = responseLBS
  status
  (toList (insert hContentType "application/json" (fromList headers)))
  (encode value)

badRequest :: Action
badRequest = return (json status400 [] Null)

notFound :: Action
notFound = return (json status404 [] Null)

notAllowed :: Action
notAllowed = return (json status405 [] Null)
