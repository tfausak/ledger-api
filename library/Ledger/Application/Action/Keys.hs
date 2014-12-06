module Ledger.Application.Action.Keys where

import Ledger.Application.Action.Common (Action, json, notFound)
import Ledger.Application.Action.Internal (getState)
import Ledger.Application.Model (newKey)
import Ledger.Application.State.Internal (queryKey, updateKeys)
import Ledger.Application.Transformer (toKeyOutput)

import Control.Monad.IO.Class (liftIO)
import Data.IxSet (insert)
import Data.Text (Text)
import Network.HTTP.Types (status200, status201)

postKeys :: Action
postKeys = do
    state <- getState
    key <- liftIO newKey
    _ <- liftIO (updateKeys state (insert key))
    return (json status201 [] (toKeyOutput key))

getKey :: Text -> Action
getKey keyId = do
    state <- getState
    maybeKey <- liftIO (queryKey state keyId)
    case maybeKey of
        Just key -> return (json status200 [] (toKeyOutput key))
        Nothing -> notFound
