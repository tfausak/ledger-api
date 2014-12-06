{-# LANGUAGE OverloadedStrings #-}

module Ledger.Application.Transformer.EntryOutput where

import Ledger.Application.Model.Entry (Entry, entryAmount, entryDescription,
                                       entryId, entryTime)

import Data.Aeson (ToJSON, object, toJSON, (.=))
import Data.Text (Text)
import Data.Time (UTCTime)

data EntryOutput = EntryOutput
    { entryOutputAmount      :: Double
    , entryOutputDescription :: Text
    , entryOutputId          :: Integer
    , entryOutputTime        :: UTCTime
    } deriving (Read, Show)

instance ToJSON EntryOutput where
    toJSON entryOutput = object
        [ "amount" .= entryOutputAmount entryOutput
        , "description" .= entryOutputDescription entryOutput
        , "id" .= entryOutputId entryOutput
        , "time" .= entryOutputTime entryOutput
        ]

toEntryOutput :: Entry -> EntryOutput
toEntryOutput entry = EntryOutput
    { entryOutputAmount = fromRational (entryAmount entry)
    , entryOutputDescription = entryDescription entry
    , entryOutputId = entryId entry
    , entryOutputTime = entryTime entry
    }
