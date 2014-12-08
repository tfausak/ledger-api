module Ledger.Application.Action.Keys where

import Ledger.Application.Action.Common (Action, json, notFound)
import Ledger.Application.Action.Internal (getState)
import Ledger.Application.Model (KeyDeleted (KeyDeleted), keyDeleted, keyId,
                                 newKey)
import Ledger.Application.State (CreateKey (CreateKey))
import Ledger.Application.State.Internal (queryKey, updateKeys)
import Ledger.Application.Transformer (toKeyOutput)

import Control.Monad.IO.Class (liftIO)
import Data.Acid (update)
import Data.IxSet (updateIx)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Network.HTTP.Types (status200, status201)

postKeysA :: Action
postKeysA = do
    state <- getState
    key <- liftIO newKey
    key' <- liftIO (update state (CreateKey key))
    let keyOutput = toKeyOutput key'
    return (json status201 [] keyOutput)

getKeyA :: Text -> Action
getKeyA kid = do
    state <- getState
    maybeKey <- liftIO (queryKey state kid)
    case maybeKey of
        Just key -> return (json status200 [] (toKeyOutput key))
        Nothing -> notFound

deleteKeyA :: Text -> Action
deleteKeyA kid = do
    state <- getState
    maybeKey <- liftIO (queryKey state kid)
    case maybeKey of
        Just key -> do
            now <- liftIO getCurrentTime
            let deletedKey = key { keyDeleted = KeyDeleted (Just now) }
            _ <- liftIO (updateKeys state (updateIx (keyId key) deletedKey))
            let keyOutput = toKeyOutput deletedKey
            return (json status200 [] keyOutput)
        Nothing -> notFound
