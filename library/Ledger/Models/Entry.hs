{-# LANGUAGE OverloadedStrings #-}

module Ledger.Models.Entry
  ( Entry (..)
  ) where

import           Data.Aeson (ToJSON, object, toJSON, (.=))
import           Data.Time  (UTCTime)

data Entry = Entry
  { amount  :: Rational
  , created :: UTCTime
  , number  :: Integer
  }

instance ToJSON Entry where
  toJSON entry = object
    [ "amount" .= (fromRational (amount entry) :: Double)
    , "created" .= created entry
    , "number" .= number entry
    ]
