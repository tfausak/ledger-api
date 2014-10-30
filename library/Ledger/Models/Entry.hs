{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}

module Ledger.Models.Entry
  ( Entry (..)
  , queryEntries
  , writeEntries
  ) where

import           Control.Monad.Reader (ask)
import           Control.Monad.State  (put)
import           Data.Acid            (Query, QueryEvent, Update, UpdateEvent)
import           Data.Acid.Advanced   (Event (QueryEvent, UpdateEvent),
                                       IsAcidic,
                                       Method (MethodResult, MethodState),
                                       acidEvents)
import           Data.Aeson           (ToJSON, object, toJSON, (.=))
import           Data.SafeCopy        (SafeCopy, contain, getCopy, putCopy,
                                       safeGet, safePut)
import           Data.Time            (UTCTime)
import           Data.Typeable        (Typeable)

data Entry = Entry
  { amount  :: Rational
  , created :: UTCTime
  , number  :: Integer
  } deriving (Typeable)

instance ToJSON Entry where
  toJSON entry = object
    [ "amount" .= (fromRational (amount entry) :: Double)
    , "created" .= created entry
    , "number" .= number entry
    ]

instance SafeCopy Entry where
  getCopy = contain $ do
    entryAmount <- safeGet
    entryCreated <- safeGet
    entryNumber <- safeGet
    return Entry
      { amount = entryAmount
      , created = entryCreated
      , number = entryNumber
      }
  putCopy entry = contain $ do
    safePut (amount entry)
    safePut (created entry)
    safePut (number entry)

queryEntries :: Query [Entry] [Entry]
queryEntries = ask

writeEntries :: [Entry] -> Update [Entry] ()
writeEntries = put

data QueryEntries = QueryEntries
  deriving (Typeable)
instance QueryEvent QueryEntries
instance Method QueryEntries where
  type MethodResult QueryEntries = [Entry]
  type MethodState QueryEntries = [Entry]
instance SafeCopy QueryEntries where
  getCopy = contain (return QueryEntries)
  putCopy QueryEntries = contain (return ())

data WriteEntries = WriteEntries [Entry]
  deriving (Typeable)
instance UpdateEvent WriteEntries
instance Method WriteEntries where
  type MethodResult WriteEntries = ()
  type MethodState WriteEntries = [Entry]
instance SafeCopy WriteEntries where
  getCopy = contain $ do
    entries <- safeGet
    return (WriteEntries entries)
  putCopy (WriteEntries entries) = contain (safePut entries)

instance IsAcidic [Entry] where
  acidEvents =
    [ UpdateEvent (\ (WriteEntries entries) -> writeEntries entries)
    , QueryEvent (\ QueryEntries -> queryEntries)
    ]
