{-# LANGUAGE OverloadedStrings #-}

module Ledger.Models.Entry.Request
  ( EntryRequest (..)
  , toEntry
  ) where

import qualified Ledger.Models.Entry as Entry

import           Control.Monad       (mzero)
import           Data.Aeson          (FromJSON, Value (Object), parseJSON, (.:))
import qualified Data.Map            as Map
import           Data.Text           (Text)
import           Data.Time           (getCurrentTime)

data EntryRequest = EntryRequest
  { amount :: Double
  , name   :: Text
  }

instance FromJSON EntryRequest where
  parseJSON (Object object) = do
    requestAmount <- object .: "amount"
    requestName <- object .: "name"
    return EntryRequest
      { amount = requestAmount
      , name = requestName
      }
  parseJSON _ = mzero

toEntry :: Entry.Entries -> EntryRequest -> IO Entry.Entry
toEntry entries entryRequest = do
  created <- getCurrentTime
  let number = if Map.null entries
        then 1
        else 1 + fst (Map.findMax entries)
  let entry = Entry.Entry
        { Entry.amount = realToFrac (amount entryRequest)
        , Entry.created = created
        , Entry.deleted = Nothing
        , Entry.name = name entryRequest
        , Entry.number = number
        }
  return entry
