{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}

module Ledger.Application.Model.Key where

import Data.Data (Data)
import Data.IxSet (Indexable, Proxy (Proxy), empty, ixGen, ixSet)
import Data.Ord (comparing)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Text (Text, pack)
import Data.Time (UTCTime (UTCTime), fromGregorian, getCurrentTime, utctDay,
                  utctDayTime)
import Data.Typeable (Typeable)
import System.Random (newStdGen, randomRs)

type KeyDeleted = Maybe UTCTime
type KeyId = Text

data Key = Key
    { keyCreated :: UTCTime
    , keyDeleted :: KeyDeleted
    , keyId      :: KeyId
    } deriving (Data, Eq, Read, Show, Typeable)

instance Indexable Key where
    empty = ixSet
        [ ixGen (Proxy :: Proxy KeyDeleted)
        , ixGen (Proxy :: Proxy KeyId)
        ]

instance Ord Key where
    compare = comparing keyCreated

$(deriveSafeCopy 1 'base ''Key)

defaultKey :: Key
defaultKey = Key
    { keyCreated = UTCTime
        { utctDay = fromGregorian 2000 1 1
        , utctDayTime = 0
        }
    , keyDeleted = Nothing
    , keyId = pack ""
    }

newKey :: IO Key
newKey = do
    now <- getCurrentTime
    generator <- newStdGen
    return defaultKey
        { keyCreated = now
        , keyId = pack (take 8 (randomRs ('a', 'z') generator))
        }
