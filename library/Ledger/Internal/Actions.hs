{-# LANGUAGE OverloadedStrings #-}

module Ledger.Internal.Actions
  ( Action
  , json
  , notAllowed
  , notFound
  ) where

import           Ledger.Internal.Main (State)

import           Control.Monad.Reader (ReaderT)
import           Data.Aeson           (ToJSON, Value (Null), encode)
import           Data.Map             (fromList, insert, toList)
import           Network.HTTP.Types   (ResponseHeaders, Status, hContentType,
                                       status404, status405)
import           Network.Wai          (Request, Response, responseLBS)

type Action = ReaderT (Request, State) IO Response

json :: (ToJSON a) => Status -> ResponseHeaders -> a -> Response
json status headers value = responseLBS
  status
  (toList (insert hContentType "application/json" (fromList headers)))
  (encode value)

notAllowed :: Action
notAllowed = return (json status405 [] Null)

notFound :: Action
notFound = return (json status404 [] Null)
