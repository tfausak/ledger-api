{-# LANGUAGE OverloadedStrings #-}

module Ledger.Application.Action.Common where

import Ledger.Application.State (State)

import Control.Monad.Reader (ReaderT)
import Data.Acid (AcidState)
import Data.Aeson (ToJSON, Value (Null), encode)
import Data.Map (fromList, insert, toList)
import Network.HTTP.Types (ResponseHeaders, Status, hContentType, status400,
                           status403, status404, status405)
import Network.Wai (Request, Response, responseLBS)

type Action = ReaderT (Request, AcidState State) IO Response

json :: (ToJSON a) => Status -> ResponseHeaders -> a -> Response
json status headers value = responseLBS
    status
    (toList (insert hContentType "application/json" (fromList headers)))
    (encode value)

badRequest :: Action
badRequest = return (json status400 [] Null)

forbidden :: Action
forbidden = return (json status403 [] Null)

notAllowed :: Action
notAllowed = return (json status405 [] Null)

notFound :: Action
notFound = return (json status404 [] Null)
