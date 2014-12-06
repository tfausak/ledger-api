{-# LANGUAGE OverloadedStrings #-}

module Ledger.Application.Transformer.EntryInput where

import Ledger.Application.Model (Entry, entryAmount, entryDescription,
                                 entryTime)

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:))
import Data.Text (Text)
import Data.Time (UTCTime)

data EntryInput = EntryInput
    { entryInputAmount      :: Double
    , entryInputDescription :: Text
    , entryInputTime        :: UTCTime
    } deriving (Read, Show)

instance FromJSON EntryInput where
    parseJSON (Object object) = EntryInput
        <$> object .: "amount"
        <*> object .: "description"
        <*> object .: "time"
    parseJSON _ = mzero

fromEntryInput :: Entry -> EntryInput -> Entry
fromEntryInput entry entryInput = entry
    { entryAmount = toRational (entryInputAmount entryInput)
    , entryDescription = entryInputDescription entryInput
    , entryTime = entryInputTime entryInput
    }
