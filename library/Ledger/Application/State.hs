{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Ledger.Application.State where

import Ledger.Application.Model

import Control.Monad.Reader (ask)
import Control.Monad.State (gets, modify)
import Data.Acid (Query, Update, liftQuery, makeAcidic)
import Data.IxSet (IxSet, Proxy (Proxy), empty, getEQ, getOne, toDescList,
                   updateIx)
import Data.Maybe (listToMaybe)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Time (UTCTime)

type Entries = IxSet Entry
type Keys = IxSet Key

data State = State
    { stateEntries :: Entries
    , stateKeys    :: Keys
    } deriving (Read, Show)

defaultState :: State
defaultState = State
    { stateEntries = empty
    , stateKeys = empty
    }

-- * Keys

queryKeys :: Query State Keys
queryKeys = fmap stateKeys ask

getKey :: KeyId -> Query State (Maybe Key)
getKey kid = do
    keys <- queryKeys
    let maybeKey = getOne (getEQ kid keys)
    return maybeKey

updateKeys :: Keys -> Update State Keys
updateKeys keys = do
    _ <- modify (\ state -> state { stateKeys = keys })
    gets stateKeys

upsertKey :: Key -> Update State Key
upsertKey key = do
    oldKeys <- liftQuery queryKeys
    let keys = updateIx (keyId key) key oldKeys
    _ <- updateKeys keys
    return key

deleteKey :: Key -> UTCTime -> Update State Key
deleteKey oldKey time = do
    let key = oldKey { keyDeleted = KeyDeleted (Just time) }
    upsertKey key

-- * Entries

queryEntries :: Query State Entries
queryEntries = fmap stateEntries ask

getEntry :: EntryId -> Query State (Maybe Entry)
getEntry eid = do
    entries <- queryEntries
    let maybeEntry = getOne (getEQ eid entries)
    return maybeEntry

updateEntries :: Entries -> Update State Entries
updateEntries entries = do
    _ <- modify (\ state -> state { stateEntries = entries })
    gets stateEntries

upsertEntry :: Entry -> Update State Entry
upsertEntry entry = do
    oldEntries <- liftQuery queryEntries
    let entries = updateIx (entryId entry) entry oldEntries
    _ <- updateEntries entries
    return entry

createEntry :: Entry -> Update State Entry
createEntry entry = do
    oldEntries <- liftQuery queryEntries
    let maxEntryId = maybe 0 entryId (listToMaybe (toDescList (Proxy :: Proxy EntryId) oldEntries))
    let entry' = entry { entryId = succ maxEntryId }
    upsertEntry entry'

deleteEntry :: Entry -> UTCTime -> Update State Entry
deleteEntry oldEntry time = do
    let entry = oldEntry { entryDeleted = EntryDeleted (Just time) }
    upsertEntry entry

-- TH

$(deriveSafeCopy 1 'base ''State)

$(makeAcidic ''State
    [ 'queryKeys
    , 'getKey
    , 'updateKeys
    , 'upsertKey
    , 'deleteKey

    , 'queryEntries
    , 'getEntry
    , 'updateEntries
    , 'upsertEntry
    , 'createEntry
    , 'deleteEntry
    ])
