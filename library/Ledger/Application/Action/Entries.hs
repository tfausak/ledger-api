{-# LANGUAGE OverloadedStrings #-}

module Ledger.Application.Action.Entries
    ( getEntriesA
    , postEntriesA
    , getEntryA
    , putEntryA
    , deleteEntryA
    ) where

import Ledger.Application.Action.Common
import Ledger.Application.Action.Internal
import Ledger.Application.Model
import Ledger.Application.State hiding (getEntry, getKey)
import Ledger.Application.Transformer

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT)
import Data.Acid (AcidState, query, update)
import Data.Aeson (decode)
import Data.ByteString.Lazy (fromStrict)
import Data.IxSet (getEQ, toList)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Read (decimal)
import Data.Time (getCurrentTime)
import qualified Network.HTTP.Types as HTTP
import Network.Wai (Request, queryString, requestBody)

getEntriesA :: Action
getEntriesA =
    withKey $ \ key -> do
        entries <- getEntries key
        let entriesList = toList entries
        let entryOutputs = map toEntryOutput entriesList
        json HTTP.status200 entryOutputs

postEntriesA :: Action
postEntriesA =
    withKey $ \ key ->
    withEntryInput $ \ entryInput -> do
        state <- getState
        entry <- liftIO newEntry
        let entry' = fromEntryInput (entry { entryKey = key }) entryInput
        entry'' <- liftIO (update state (CreateEntry entry'))
        let entryOutput = toEntryOutput entry''
        json HTTP.status201 entryOutput

getEntryA :: Text -> Action
getEntryA eid =
    withKey $ \ key ->
    withEntry key eid $ \ entry -> do
        let entryOutput = toEntryOutput entry
        json HTTP.status200 entryOutput

putEntryA :: Text -> Action
putEntryA eid =
    withKey $ \ key ->
    withEntry key eid $ \ entry ->
    withEntryInput $ \ entryInput -> do
        state <- getState
        let entry' = fromEntryInput entry entryInput
        entry'' <- liftIO (update state (UpsertEntry entry'))
        let entryOutput = toEntryOutput entry''
        json HTTP.status201 entryOutput

deleteEntryA :: Text -> Action
deleteEntryA eid =
    withKey $ \ key ->
    withEntry key eid $ \ entry -> do
        state <- getState
        deletedEntry <- liftIO $ do
            now <- getCurrentTime
            update state (DeleteEntry entry now)
        let entryOutput = toEntryOutput deletedEntry
        json HTTP.status200 entryOutput

--

withKey :: (Key -> Action) -> Action
withKey action = do
    maybeKey <- getKey
    case maybeKey of
        Just key -> case keyDeleted key of
            KeyDeleted (Just _) -> goneA
            KeyDeleted Nothing -> action key
        Nothing -> forbiddenA

getKey :: (MonadIO m) => ReaderT (Request, AcidState State) m (Maybe Key)
getKey = do
    maybeKeyId <- getKeyId
    case maybeKeyId of
        Just kid -> do
            state <- getState
            liftIO (query state (GetKey kid))
        Nothing -> return Nothing

getKeyId :: (MonadIO m) => ReaderT (Request, a) m (Maybe KeyId)
getKeyId = do
    request <- getRequest
    case lookup "key" (queryString request) of
        Just (Just kid) -> return (Just (decodeUtf8 kid))
        _ -> return Nothing

--

withEntry :: Key -> Text -> (Entry -> Action) -> Action
withEntry key eid action = do
    maybeEntry <- getEntry key eid
    case maybeEntry of
        Just entry -> case entryDeleted entry of
            EntryDeleted (Just _) -> goneA
            EntryDeleted Nothing -> action entry
        Nothing -> notFoundA

getEntry :: (MonadIO m) => Key -> Text -> ReaderT (a, AcidState State) m (Maybe Entry)
getEntry key eid = do
    state <- getState
    let eitherEid = decimal eid
    case eitherEid of
        Left _ -> return Nothing
        Right (eid', _) -> do
            maybeEntry <- liftIO (query state (GetEntry eid'))
            case maybeEntry of
                Just entry -> if entryKey entry == key
                    then return (Just entry)
                    else return Nothing
                Nothing -> return Nothing

getEntries :: (MonadIO m) => Key -> ReaderT (a, AcidState State) m Entries
getEntries key = do
    state <- getState
    allEntries <- liftIO (query state QueryEntries)
    let notDeletedEntries = getEQ (EntryDeleted Nothing) allEntries
    let entries = getEQ key notDeletedEntries
    return entries

withEntryInput :: (EntryInput -> Action) -> Action
withEntryInput action = do
    maybeEntryInput <- getEntryInput
    case maybeEntryInput of
        Just entryInput -> action entryInput
        Nothing -> badRequestA

getEntryInput :: (MonadIO m) => ReaderT (Request, a) m (Maybe EntryInput)
getEntryInput = do
    request <- getRequest
    body <- liftIO (requestBody request)
    let maybeEntryInput = decode (fromStrict body)
    return maybeEntryInput
