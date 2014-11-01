{-# LANGUAGE OverloadedStrings #-}

module Ledger.Models.Entry.Response
  ( EntryResponse (..)
  , toResponse
  ) where

import qualified Ledger.Models.Entry as Entry

import           Data.Aeson          (ToJSON, object, toJSON, (.=))
import           Data.Text           (Text)
import           Data.Time           (UTCTime)

data EntryResponse = EntryResponse
  { amount  :: Double
  , created :: UTCTime
  , name    :: Text
  , number  :: Integer
  }

instance ToJSON EntryResponse where
  toJSON entryResponse = object
    [ "amount" .= amount entryResponse
    , "created" .= created entryResponse
    , "name" .= name entryResponse
    , "number" .= number entryResponse
    ]

toResponse :: Entry.Entry -> EntryResponse
toResponse entry = EntryResponse
  { amount = fromRational (Entry.amount entry)
  , created = Entry.created entry
  , name = Entry.name entry
  , number = Entry.number entry
  }
