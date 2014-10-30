{-# LANGUAGE OverloadedStrings #-}

module Ledger.Internal.Actions
  ( Action
  , json
  ) where

import           Ledger.Models        (Entry)

import           Control.Monad.Reader (ReaderT)
import           Data.Acid            (AcidState)
import           Data.Aeson           (ToJSON, encode)
import           Data.Map             (alter, fromList, toList)
import           Network.HTTP.Types   (ResponseHeaders, Status, hContentType)
import           Network.Wai          (Request, Response, responseLBS)

type Action = ReaderT (Request, AcidState [Entry]) IO Response

json :: (ToJSON a) => Status -> ResponseHeaders -> a -> Response
json status headers value = responseLBS
  status
  (toList (alter (const (Just "application/json")) hContentType (fromList headers)))
  (encode value)
