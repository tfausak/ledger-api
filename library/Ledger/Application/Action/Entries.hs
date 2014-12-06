module Ledger.Application.Action.Entries where

import Ledger.Application.Action.Common (Action, json)
import Ledger.Application.Action.Internal (getState, withEntry, withEntryInput,
                                           withKey)
import Ledger.Application.State.Internal (createEntry, queryEntriesForKey,
                                          updateEntry)
import qualified Ledger.Application.State.Internal as Internal (deleteEntry)
import Ledger.Application.Transformer (fromEntryInput, toEntryOutput)

import Control.Monad.IO.Class (liftIO)
import Data.IxSet (toList)
import Data.Text (Text)
import Network.HTTP.Types (status200, status201)

getEntries :: Action
getEntries =
    withKey $ \ key -> do
        state <- getState
        entries <- liftIO (queryEntriesForKey state key)
        let entryOutputs = map toEntryOutput (toList entries)
        return (json status200 [] entryOutputs)

postEntries :: Action
postEntries =
    withKey $ \ key ->
    withEntryInput $ \ entryInput -> do
        state <- getState
        entry <- liftIO (createEntry state key entryInput)
        let entryOutput = toEntryOutput entry
        return (json status201 [] entryOutput)

getEntry :: Text -> Action
getEntry entryId =
    withKey $ \ key ->
    withEntry key entryId $ \ entry -> do
        let entryOutput = toEntryOutput entry
        return (json status200 [] entryOutput)

putEntry :: Text -> Action
putEntry entryId =
    withKey $ \ key ->
    withEntry key entryId $ \ entry ->
    withEntryInput $ \ entryInput -> do
        state <- getState
        let update = flip fromEntryInput entryInput
        updatedEntry <- liftIO (updateEntry state entry update)
        let entryOutput = toEntryOutput updatedEntry
        return (json status201 [] entryOutput)

deleteEntry :: Text -> Action
deleteEntry entryId =
    withKey $ \ key ->
    withEntry key entryId $ \ entry -> do
        state <- getState
        deletedEntry <- liftIO (Internal.deleteEntry state entry)
        let entryOutput = toEntryOutput deletedEntry
        return (json status200 [] entryOutput)
