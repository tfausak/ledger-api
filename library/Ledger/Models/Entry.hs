module Ledger.Models.Entry
  ( Entry (..)
  ) where

import           Data.Aeson (ToJSON, object, toJSON)

data Entry = Entry

instance ToJSON Entry where
  toJSON _entry = object
    [
    ]
