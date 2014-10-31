module Ledger.Actions.Entries
  ( getEntries
  , postEntries
  , getEntry
  , putEntry
  ) where

import           Ledger.Internal.Actions (Action, badRequest, json, notFound)
import           Ledger.Models           (QueryEntries (..), WriteEntries (..),
                                          entryFromRequest, entryToResponse,
                                          updateEntryFromRequest)
import qualified Ledger.Models.Entry     as Entry

import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Reader    (asks)
import           Data.Acid               (query, update)
import           Data.Aeson              (decode)
import           Data.List               (find)
import           Data.Text               (unpack)
import           Network.HTTP.Types      (status200)
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
    Nothing -> badRequest
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
    Nothing -> notFound
    Just number -> do
      state <- asks snd
      entries <- liftIO (query state QueryEntries)
      let maybeEntry = find (\ entry -> Entry.number entry == number) entries
      case maybeEntry of
        Nothing -> notFound
        Just entry -> do
          let entryResponse = entryToResponse entry
          return (json status200 [] entryResponse)

putEntry :: Action
putEntry = do
  -- get entry number from request parameters
  request <- asks fst
  let parameter = pathInfo request !! 1
  let maybeNumber = readMaybe (unpack parameter)
  case maybeNumber of
    Nothing -> notFound
    Just number -> do
      -- find entry in state
      state <- asks snd
      entries <- liftIO (query state QueryEntries)
      let maybeEntry = find (\ entry -> Entry.number entry == number) entries
      case maybeEntry of
        Nothing -> notFound
        Just oldEntry -> do
          -- parse entry from request body
          body <- liftIO (strictRequestBody request)
          let maybeEntryRequest = decode body
          case maybeEntryRequest of
            Nothing -> badRequest
            Just entryRequest -> do
              -- create a new updated entry
              entry <- liftIO $ do
                oldEntries <- query state QueryEntries
                let newEntry = updateEntryFromRequest oldEntry entryRequest
                let newEntries = map
                      (\ e -> if Entry.number e == number then newEntry else e)
                      oldEntries
                update state (WriteEntries newEntries)
                return newEntry
              let entryResponse = entryToResponse entry
              return (json status200 [] entryResponse)
