module Ledger.Application.State.Internal where

import Ledger.Application.Model (Entry, EntryDeleted (EntryDeleted), EntryId,
                                 Key, KeyDeleted (KeyDeleted), KeyId,
                                 entryDeleted, entryId, entryKey, newEntry)
import Ledger.Application.State (Entries, Keys, QueryEntries (QueryEntries),
                                 QueryKeys (QueryKeys), State,
                                 UpdateEntries (UpdateEntries),
                                 UpdateKeys (UpdateKeys))
import Ledger.Application.Transformer (EntryInput, fromEntryInput)

import Control.Monad.IO.Class (liftIO)
import Data.Acid (AcidState, query, update)
import Data.IxSet (getEQ, getOne, insert, updateIx)
import Data.Time (getCurrentTime)

createEntry :: AcidState State -> Key -> EntryInput -> IO Entry
createEntry state key entryInput = do
    entry <- newEntry
    let entryWithKey = entry { entryKey = key }
    let fullEntry = fromEntryInput entryWithKey entryInput
    _ <- updateEntries state (insert fullEntry)
    return fullEntry

deleteEntry :: AcidState State -> Entry -> IO Entry
deleteEntry state entry = do
    now <- getCurrentTime
    let deletedEntry = entry { entryDeleted = EntryDeleted (Just now) }
    _ <- updateEntries state (updateIx (entryId entry) deletedEntry)
    return deletedEntry

queryEntries :: AcidState State -> IO Entries
queryEntries state = query state QueryEntries

queryEntriesForKey :: AcidState State -> Key -> IO Entries
queryEntriesForKey state key = do
    allEntries <- liftIO (queryEntries state)
    let notDeletedEntries = getEQ (EntryDeleted Nothing) allEntries
    let entries = getEQ key notDeletedEntries
    return entries

queryEntry :: AcidState State -> Key -> Either a (EntryId, b) -> IO (Maybe Entry)
queryEntry state key (Right (eid, _)) = do
    entries <- queryEntriesForKey state key
    return (getOne (getEQ eid entries))
queryEntry _ _ _ = return Nothing

queryKey :: AcidState State -> KeyId -> IO (Maybe Key)
queryKey state keyId = do
    allKeys <- queryKeys state
    let notDeletedKeys = getEQ (KeyDeleted Nothing) allKeys
    let keys = getEQ keyId notDeletedKeys
    let maybeKey = getOne keys
    return maybeKey

queryKeys :: AcidState State -> IO Keys
queryKeys state = query state QueryKeys

updateEntry :: AcidState State -> Entry -> (Entry -> Entry) -> IO Entry
updateEntry state entry f = do
    let updatedEntry = f entry
    _ <- updateEntries state (updateIx (entryId entry) updatedEntry)
    return updatedEntry

updateEntries :: AcidState State -> (Entries -> Entries) -> IO Entries
updateEntries state f = do
    entries <- queryEntries state
    let updatedEntries = f entries
    update state (UpdateEntries updatedEntries)
    return updatedEntries

updateKeys :: AcidState State -> (Keys -> Keys) -> IO Keys
updateKeys state f = do
    keys <- queryKeys state
    let updatedKeys = f keys
    update state (UpdateKeys updatedKeys)
    return updatedKeys
