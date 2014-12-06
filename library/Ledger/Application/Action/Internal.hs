{-# LANGUAGE OverloadedStrings #-}

module Ledger.Application.Action.Internal where

import Ledger.Application.Action.Common (Action, badRequest, forbidden,
                                         notFound)
import Ledger.Application.Model (Entry, Key, KeyId)
import Ledger.Application.State (State)
import Ledger.Application.State.Internal (queryEntry, queryKey)
import Ledger.Application.Transformer (EntryInput)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, asks)
import Data.Acid (AcidState)
import Data.Aeson (decode)
import Data.ByteString.Lazy (fromStrict)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Read (decimal)
import Network.Wai (Request, queryString, requestBody)

getEntry :: (MonadIO m) => Key -> Text -> ReaderT (a, AcidState State) m (Maybe Entry)
getEntry key entryId = do
    state <- getState
    let eitherEntryId = decimal entryId
    liftIO (queryEntry state key eitherEntryId)

getEntryInput :: (MonadIO m) => ReaderT (Request, a) m (Maybe EntryInput)
getEntryInput = do
    request <- getRequest
    body <- liftIO (requestBody request)
    let maybeEntryInput = decode (fromStrict body)
    return maybeEntryInput

getKey :: (MonadIO m) => ReaderT (Request, AcidState State) m (Maybe Key)
getKey = do
    state <- getState
    maybeKeyId <- getKeyId
    case maybeKeyId of
        Just keyId -> liftIO (queryKey state keyId)
        Nothing -> return Nothing

getKeyId :: (Monad m) => ReaderT (Request, a) m (Maybe KeyId)
getKeyId = do
    request <- getRequest
    let query = queryString request
    let maybeKeyId = lookup "key" query
    let keyId = case maybeKeyId of
            Just (Just keyId') -> Just (decodeUtf8 keyId')
            _ -> Nothing
    return keyId

getRequest :: (Monad m) => ReaderT (Request, a) m Request
getRequest = asks fst

getState :: (Monad m) => ReaderT (a, AcidState State) m (AcidState State)
getState = asks snd

withEntry :: Key -> Text -> (Entry -> Action) -> Action
withEntry key entryId' action = do
    maybeEntry <- getEntry key entryId'
    case maybeEntry of
        Just entry -> action entry
        Nothing -> notFound

withEntryInput :: (EntryInput -> Action) -> Action
withEntryInput action = do
    maybeEntryInput <- getEntryInput
    case maybeEntryInput of
        Just entryInput -> action entryInput
        Nothing -> badRequest

withKey :: (Key -> Action) -> Action
withKey action = do
    maybeKey <- getKey
    case maybeKey of
        Just key -> action key
        Nothing -> forbidden
