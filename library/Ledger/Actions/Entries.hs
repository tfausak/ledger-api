module Ledger.Actions.Entries
  ( getEntries
  , postEntries
  , getEntry
  , putEntry
  , deleteEntry
  ) where

import           Ledger.Actions.Common   (badRequest, notFound)
import           Ledger.Internal.Actions (json)
import           Ledger.Models           (QueryEntries (..), WriteEntries (..),
                                          entryFromRequest, entryToResponse,
                                          updateEntryFromRequest)
import qualified Ledger.Models.Entry     as Entry
import           Ledger.Types            (Action)

import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Reader    (asks)
import           Data.Acid               (query, update)
import           Data.Aeson              (Value (Null), decode)
import           Data.List               (find)
import           Data.Maybe              (isNothing)
import           Data.Text               (unpack)
import           Data.Time               (getCurrentTime)
import           Network.HTTP.Types      (status200)
import           Network.Wai             (pathInfo, strictRequestBody)
import           Text.Read               (readMaybe)

getEntries :: Action
getEntries = do
  state <- asks snd
  allEntries <- liftIO (query state QueryEntries)
  let entries = filter (\ entry -> isNothing (Entry.deleted entry)) allEntries
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
      allEntries <- liftIO (query state QueryEntries)
      let entries = filter (\ entry -> isNothing (Entry.deleted entry)) allEntries
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
      allEntries <- liftIO (query state QueryEntries)
      let entries = filter (\ entry -> isNothing (Entry.deleted entry)) allEntries
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

deleteEntry :: Action
deleteEntry = do
  -- get entry number from request parameters
  request <- asks fst
  let parameter = pathInfo request !! 1
  let maybeNumber = readMaybe (unpack parameter)
  case maybeNumber of
    Nothing -> notFound
    Just number -> do
      -- find entry in state
      state <- asks snd
      allEntries <- liftIO (query state QueryEntries)
      let entries = filter (\ entry -> isNothing (Entry.deleted entry)) allEntries
      let maybeEntry = find (\ entry -> Entry.number entry == number) entries
      case maybeEntry of
        Nothing -> notFound
        Just oldEntry -> do
          -- delete the entry
          _ <- liftIO $ do
            oldEntries <- query state QueryEntries
            now <- getCurrentTime
            let newEntry = oldEntry { Entry.deleted = Just now }
            let newEntries = map
                  (\ e -> if Entry.number e == number then newEntry else e)
                  oldEntries
            update state (WriteEntries newEntries)
          return (json status200 [] Null)
