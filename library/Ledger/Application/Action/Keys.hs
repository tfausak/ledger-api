module Ledger.Application.Action.Keys
    ( postKeysA
    , getKeyA
    , deleteKeyA
    ) where

import Ledger.Application.Action.Common
import Ledger.Application.Action.Internal
import Ledger.Application.Model
import Ledger.Application.State
import Ledger.Application.Transformer

import Control.Monad.IO.Class (liftIO)
import Data.Acid (query, update)
import Data.Time (getCurrentTime)
import qualified Network.HTTP.Types as HTTP

postKeysA :: Action
postKeysA = do
    state <- getState
    key <- liftIO $ do
        k <- newKey
        update state (UpsertKey k)
    let keyOutput = toKeyOutput key
    json HTTP.status201 keyOutput

getKeyA :: KeyId -> Action
getKeyA kid =
    withKey kid $ \ key -> do
        let keyOutput = toKeyOutput key
        json HTTP.status200 keyOutput

deleteKeyA :: KeyId -> Action
deleteKeyA kid =
    withKey kid $ \ key -> do
        state <- getState
        deletedKey <- liftIO $ do
            now <- getCurrentTime
            update state (DeleteKey key now)
        let keyOutput = toKeyOutput deletedKey
        json HTTP.status200 keyOutput

--

withKey :: KeyId -> (Key -> Action) -> Action
withKey kid action = do
    state <- getState
    maybeKey <- liftIO (query state (GetKey kid))
    case maybeKey of
        Just key -> case keyDeleted key of
            KeyDeleted (Just _) -> goneA
            KeyDeleted Nothing -> action key
        Nothing -> notFoundA
