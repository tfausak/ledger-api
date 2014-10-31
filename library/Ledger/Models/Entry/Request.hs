{-# LANGUAGE OverloadedStrings #-}

module Ledger.Models.Entry.Request
  ( EntryRequest (..)
  , toEntry
  ) where

import qualified Ledger.Models.Entry as Entry

import           Control.Monad       (mzero)
import           Data.Aeson          (FromJSON, Value (Object), parseJSON, (.:))
import           Data.List           (genericLength)
import           Data.Time           (getCurrentTime)

data EntryRequest = EntryRequest
  { amount :: Double
  }

instance FromJSON EntryRequest where
  parseJSON (Object object) = do
    requestAmount <- object .: "amount"
    return EntryRequest
      { amount = requestAmount
      }
  parseJSON _ = mzero

toEntry :: [Entry.Entry] -> EntryRequest -> IO Entry.Entry
toEntry entries entryRequest = do
  created <- getCurrentTime
  let number = 1 + genericLength entries
  let entry = Entry.Entry
        { Entry.amount = realToFrac (amount entryRequest)
        , Entry.created = created
        , Entry.deleted = Nothing
        , Entry.number = number
        }
  return entry
