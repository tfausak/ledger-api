module Ledger.Actions.Entries
  ( getEntries
  , postEntries
  ) where

import           Ledger.Internal.Actions (Action, json)
import           Ledger.Models           (QueryEntries (..), WriteEntries (..),
                                          entryFromRequest, entryToResponse)

import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Reader    (asks)
import           Data.Acid               (query, update)
import           Data.Aeson              (Value (Null), decode)
import           Network.HTTP.Types      (status200, status400)
import           Network.Wai             (strictRequestBody)

getEntries :: Action
getEntries = do
  state <- asks snd
  entries <- liftIO (query state QueryEntries)
  let entryResponses = map entryToResponse entries
  return (json status200 [] entryResponses)

postEntries :: Action
postEntries = do
  request <- asks fst
  body <- liftIO (strictRequestBody request)
  let maybeEntryRequest = decode body
  case maybeEntryRequest of
    Nothing -> do
      let response = json status400 [] Null
      return response
    Just entryRequest -> do
      state <- asks snd
      entry <- liftIO $ do
        oldEntries <- query state QueryEntries
        entry <- entryFromRequest oldEntries entryRequest
        let newEntries = entry : oldEntries
        update state (WriteEntries newEntries)
        return entry
      let entryResponse = entryToResponse entry
      return (json status200 [] entryResponse)
