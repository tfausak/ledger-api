{-# LANGUAGE OverloadedStrings #-}

module Ledger.Models.Entry.Request
  ( EntryRequest (..)
  , toEntry
  ) where

import qualified Ledger.Models.Entry as Entry

import           Control.Monad       (mzero)
import           Data.Aeson          (FromJSON, Value (Object), parseJSON, (.:))
import qualified Data.Map            as Map
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
        , Entry.number = number
        }
  return entry
