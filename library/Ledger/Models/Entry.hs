{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}

module Ledger.Models.Entry where

import           Control.Monad.Reader (ask)
import           Control.Monad.State  (put)
import           Data.Acid            (Query, QueryEvent, Update, UpdateEvent)
import           Data.Acid.Advanced   (Event (QueryEvent, UpdateEvent),
                                       IsAcidic,
                                       Method (MethodResult, MethodState),
                                       acidEvents)
import           Data.Map             (Map)
import           Data.SafeCopy        (SafeCopy, contain, getCopy, putCopy,
                                       safeGet, safePut)
import           Data.Text            (Text)
import           Data.Time            (UTCTime)
import           Data.Typeable        (Typeable)

type Entries = Map Integer Entry

data Entry = Entry
  { amount  :: Rational
  , created :: UTCTime
  , deleted :: Maybe UTCTime
  , name    :: Text
  , number  :: Integer
  } deriving (Typeable)

instance SafeCopy Entry where
  getCopy = contain $ do
    entryAmount <- safeGet
    entryCreated <- safeGet
    entryDeleted <- safeGet
    entryName <- safeGet
    entryNumber <- safeGet
    return Entry
      { amount = entryAmount
      , created = entryCreated
      , deleted = entryDeleted
      , name = entryName
      , number = entryNumber
      }
  putCopy entry = contain $ do
    safePut (amount entry)
    safePut (created entry)
    safePut (deleted entry)
    safePut (name entry)
    safePut (number entry)

queryEntries :: Query Entries Entries
queryEntries = ask

writeEntries :: Entries -> Update Entries ()
writeEntries = put

data QueryEntries = QueryEntries
  deriving (Typeable)
instance QueryEvent QueryEntries
instance Method QueryEntries where
  type MethodResult QueryEntries = Entries
  type MethodState QueryEntries = Entries
instance SafeCopy QueryEntries where
  getCopy = contain (return QueryEntries)
  putCopy QueryEntries = contain (return ())

data WriteEntries = WriteEntries Entries
  deriving (Typeable)
instance UpdateEvent WriteEntries
instance Method WriteEntries where
  type MethodResult WriteEntries = ()
  type MethodState WriteEntries = Entries
instance SafeCopy WriteEntries where
  getCopy = contain $ do
    entries <- safeGet
    return (WriteEntries entries)
  putCopy (WriteEntries entries) = contain (safePut entries)

instance IsAcidic Entries where
  acidEvents =
    [ UpdateEvent (\ (WriteEntries entries) -> writeEntries entries)
    , QueryEvent (\ QueryEntries -> queryEntries)
    ]
