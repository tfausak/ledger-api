{-# LANGUAGE OverloadedStrings #-}

module Ledger.Internal.Actions
  ( Action
  , json
  ) where

import           Ledger.Internal.Main (State)

import           Control.Monad.Reader (ReaderT)
import           Data.Aeson           (ToJSON, encode)
import           Data.Map             (fromList, insert, toList)
import           Network.HTTP.Types   (ResponseHeaders, Status, hContentType)
import           Network.Wai          (Request, Response, responseLBS)

type Action = ReaderT (Request, State) IO Response

json :: (ToJSON a) => Status -> ResponseHeaders -> a -> Response
json status headers value = responseLBS
  status
  (toList (insert hContentType "application/json" (fromList headers)))
  (encode value)
