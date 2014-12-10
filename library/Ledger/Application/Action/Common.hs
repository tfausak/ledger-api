{-# LANGUAGE OverloadedStrings #-}

module Ledger.Application.Action.Common where

import Ledger.Application.State (State)

import Control.Monad.Reader (ReaderT)
import Data.Acid (AcidState)
import Data.Aeson (ToJSON, Value (Null), encode)
import Network.HTTP.Types (Status, hContentType)
import qualified Network.HTTP.Types as HTTP
import Network.Wai (Request, Response, responseLBS)

type Action = ReaderT (Request, AcidState State) IO Response

json :: (ToJSON a) => Status -> a -> Action
json status value =
    let headers = [(hContentType, "application/json")]
        response = responseLBS status headers (encode value)
    in  return response

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
