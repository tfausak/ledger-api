module Ledger.Application.Action.Entries where

import Ledger.Application.Action.Common (Action, json, notFound)
import Ledger.Application.Action.Internal (getState, withEntryInput, withKey)
import qualified Ledger.Application.Action.Internal as Internal
import Ledger.Application.Model (entryId)
import Ledger.Application.State.Internal (createEntry, queryEntriesForKey,
                                          updateEntries)
import Ledger.Application.Transformer (fromEntryInput, toEntryOutput)

import Control.Monad.IO.Class (liftIO)
import Data.IxSet (deleteIx, toList, updateIx)
import Data.Text (Text)
import Network.HTTP.Types (status200, status201)

getEntries :: Action
getEntries = withKey $ \ key -> do
    state <- getState
    entries <- liftIO (queryEntriesForKey state key)
    let entryOutputs = map toEntryOutput (toList entries)
    return (json status200 [] entryOutputs)

postEntries :: Action
postEntries = withKey $ \ key ->
    withEntryInput $ \ entryInput -> do
        state <- getState
        entry <- liftIO (createEntry state key entryInput)
        return (json status201 [] (toEntryOutput entry))

getEntry :: Text -> Action
getEntry entryId' = withKey $ \ key -> do
    maybeEntry <- Internal.getEntry key entryId'
    case maybeEntry of
        Just entry -> do
            let entryOutput = toEntryOutput entry
            return (json status200 [] entryOutput)
        Nothing -> notFound

putEntry :: Text -> Action
putEntry entryId' = withKey $ \ key -> do
    state <- getState
    maybeEntry <- Internal.getEntry key entryId'
    case maybeEntry of
        Just entry -> withEntryInput $ \ entryInput -> do
            let updatedEntry = fromEntryInput entry entryInput
            _ <- liftIO (updateEntries state (updateIx (entryId entry) updatedEntry))
            return (json status201 [] (toEntryOutput updatedEntry))
        Nothing -> notFound

deleteEntry :: Text -> Action
deleteEntry entryId' = withKey $ \ key -> do
    state <- getState
    maybeEntry <- Internal.getEntry key entryId'
    case maybeEntry of
        Just entry -> do
            _ <- liftIO (updateEntries state (deleteIx (entryId entry)))
            return (json status200 [] (toEntryOutput entry))
        Nothing -> notFound
