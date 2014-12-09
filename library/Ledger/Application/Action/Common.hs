{-# LANGUAGE OverloadedStrings #-}

module Ledger.Application.Action.Common where

import Ledger.Application.State (State)

import Control.Monad.Reader (ReaderT)
import Data.Acid (AcidState)
import Data.Aeson (ToJSON, Value (Null), encode)
import Data.Map (fromList, insert, toList)
import Network.HTTP.Types (ResponseHeaders, Status, hContentType)
import qualified Network.HTTP.Types as HTTP
import Network.Wai (Request, Response, responseLBS)

type Action = ReaderT (Request, AcidState State) IO Response

json :: (ToJSON a) => Status -> a -> Action
json status value = return (json' status [] value)

json' :: (ToJSON a) => Status -> ResponseHeaders -> a -> Response
json' status headers value = responseLBS
    status
    (toList (insert hContentType "application/json" (fromList headers)))
    (encode value)

badRequestA :: Action
badRequestA = json HTTP.status400 Null

forbiddenA :: Action
forbiddenA = json HTTP.status403 Null

notFoundA :: Action
notFoundA = json HTTP.status404 Null

notAllowedA :: Action
notAllowedA = json HTTP.status405 Null

goneA :: Action
goneA = json HTTP.status410 Null
