{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}

module Ledger.Application.Model.Entry where

import Ledger.Application.Model.Key (KeyId, defaultKey, keyId)

import Data.Data (Data)
import Data.IxSet (Indexable, Proxy (Proxy), empty, ixGen, ixSet)
import Data.Ord (comparing)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Text (Text, pack)
import Data.Time (UTCTime (UTCTime), fromGregorian, getCurrentTime, utctDay,
                  utctDayTime)
import Data.Typeable (Typeable)

newtype EntryDeleted = EntryDeleted (Maybe UTCTime)
    deriving (Data, Eq, Ord, Read, Show, Typeable)

$(deriveSafeCopy 1 'base ''EntryDeleted)

type EntryId = Integer

data Entry = Entry
    { entryAmount      :: Rational
    , entryCreated     :: UTCTime
    , entryDeleted     :: EntryDeleted
    , entryDescription :: Text
    , entryKeyId       :: KeyId
    , entryId          :: EntryId
    , entryTime        :: UTCTime
    } deriving (Data, Eq, Read, Show, Typeable)

instance Indexable Entry where
    empty = ixSet
        [ ixGen (Proxy :: Proxy EntryDeleted)
        , ixGen (Proxy :: Proxy KeyId)
        , ixGen (Proxy :: Proxy EntryId)
        ]

instance Ord Entry where
    compare = comparing entryCreated

$(deriveSafeCopy 1 'base ''Entry)

defaultEntry :: Entry
defaultEntry = Entry
    { entryAmount = 0
    , entryCreated = UTCTime
        { utctDay = fromGregorian 2000 1 1
        , utctDayTime = 0
        }
    , entryDeleted = EntryDeleted Nothing
    , entryDescription = pack ""
    , entryKeyId = keyId defaultKey
    , entryId = 0
    , entryTime = UTCTime
        { utctDay = fromGregorian 2000 1 1
        , utctDayTime = 0
        }
    }

newEntry :: IO Entry
newEntry = do
    now <- getCurrentTime
    return defaultEntry
        { entryCreated = now
        }
