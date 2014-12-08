module Ledger.Application.Action.Keys where

import Ledger.Application.Action.Common (Action, json, notFound)
import Ledger.Application.Action.Internal (getState)
import Ledger.Application.Model (KeyDeleted (KeyDeleted), keyDeleted, keyId,
                                 newKey)
import Ledger.Application.State.Internal (createKey, queryKey, updateKeys)
import Ledger.Application.Transformer (toKeyOutput)

import Control.Monad.IO.Class (liftIO)
import Data.IxSet (updateIx)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Network.HTTP.Types (status200, status201)

postKeys :: Action
postKeys = do
    state <- getState
    key <- liftIO newKey
    key' <- liftIO (createKey state key)
    let keyOutput = toKeyOutput key'
    return (json status201 [] keyOutput)

getKey :: Text -> Action
getKey kid = do
    state <- getState
    maybeKey <- liftIO (queryKey state kid)
    case maybeKey of
        Just key -> return (json status200 [] (toKeyOutput key))
        Nothing -> notFound

deleteKey :: Text -> Action
deleteKey kid = do
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
