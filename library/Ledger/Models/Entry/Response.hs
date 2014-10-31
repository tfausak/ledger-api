{-# LANGUAGE OverloadedStrings #-}

module Ledger.Models.Entry.Response
  ( EntryResponse (..)
  , toResponse
  ) where

import qualified Ledger.Models.Entry as Entry

import           Data.Aeson          (ToJSON, object, toJSON, (.=))
import           Data.Time           (UTCTime)

data EntryResponse = EntryResponse
  { amount  :: Double
  , created :: UTCTime
  , number  :: Integer
  }

instance ToJSON EntryResponse where
  toJSON entryResponse = object
    [ "amount" .= amount entryResponse
    , "created" .= created entryResponse
    , "number" .= number entryResponse
    ]

toResponse :: Entry.Entry -> EntryResponse
toResponse entry = EntryResponse
  { amount = fromRational (Entry.amount entry)
  , created = Entry.created entry
  , number = Entry.number entry
  }
