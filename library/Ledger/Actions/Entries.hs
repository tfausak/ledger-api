module Ledger.Actions.Entries
  ( getEntries
  , postEntries
  , getEntry
  ) where

import           Ledger.Internal.Actions (Action, json)
import           Ledger.Models           (QueryEntries (..), WriteEntries (..),
                                          entryFromRequest, entryToResponse)
import qualified Ledger.Models.Entry     as Entry

import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Reader    (asks)
import           Data.Acid               (query, update)
import           Data.Aeson              (Value (Null), decode)
import           Data.List               (find)
import           Data.Text               (unpack)
import           Network.HTTP.Types      (status200, status400, status404)
import           Network.Wai             (pathInfo, strictRequestBody)
import           Text.Read               (readMaybe)

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

getEntry :: Action
getEntry = do
  request <- asks fst
  let parameter = pathInfo request !! 1
  let maybeNumber = readMaybe (unpack parameter)
  case maybeNumber of
    Nothing -> return (json status404 [] Null)
    Just number -> do
      state <- asks snd
      entries <- liftIO (query state QueryEntries)
      let maybeEntry = find (\ entry -> Entry.number entry == number) entries
      case maybeEntry of
        Nothing -> return (json status404 [] Null)
        Just _ -> return (json status200 [] Null)
