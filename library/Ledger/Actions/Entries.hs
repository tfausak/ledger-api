module Ledger.Actions.Entries
  ( getEntries
  , postEntries
  , getEntry
  , putEntry
  , deleteEntry
  ) where

import           Ledger.Actions.Common        (Action, badRequest, notFound)
import           Ledger.Models.Entry          (QueryEntries (QueryEntries),
                                               WriteEntries (WriteEntries))
import qualified Ledger.Models.Entry          as Entry
import           Ledger.Models.Entry.Request  (toEntry)
import qualified Ledger.Models.Entry.Request  as EntryRequest
import           Ledger.Models.Entry.Response (toResponse)
import           Ledger.State                 (State)
import           Ledger.Utilities             (json)

import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Reader         (ReaderT, asks)
import           Data.Acid                    (query, update)
import           Data.Aeson                   (Value (Null), decode)
import           Data.List                    (find)
import qualified Data.Map                     as Map
import           Data.Maybe                   (isNothing)
import           Data.Text                    (unpack)
import           Data.Time                    (getCurrentTime)
import           Network.HTTP.Types           (status200)
import           Network.Wai                  (Request, pathInfo,
                                               strictRequestBody)
import           Text.Read                    (readMaybe)

getEntries :: Action
getEntries = do
  entries <- findEntries
  let entryResponses = map toResponse entries
  let response = json status200 [] entryResponses
  return response

postEntries :: Action
postEntries = do
  maybeEntryRequest <- decodeEntry
  case maybeEntryRequest of
    Nothing -> badRequest
    Just entryRequest -> do
      entry <- createEntry entryRequest
      let entryResponse = toResponse entry
      let response = json status200 [] entryResponse
      return response

getEntry :: Action
getEntry = do
  request <- asks fst
  let parameter = pathInfo request !! 2
  let maybeNumber = readMaybe (unpack parameter)
  case maybeNumber of
    Nothing -> notFound
    Just number -> do
      state <- asks snd
      allEntries <- liftIO (query state QueryEntries)
      let entries = filter
            (\ entry -> isNothing (Entry.deleted entry)) (Map.elems allEntries)
      let maybeEntry = find (\ entry -> Entry.number entry == number) entries
      case maybeEntry of
        Nothing -> notFound
        Just entry -> do
          let entryResponse = toResponse entry
          return (json status200 [] entryResponse)

putEntry :: Action
putEntry = do
  -- get entry number from request parameters
  request <- asks fst
  let parameter = pathInfo request !! 2
  let maybeNumber = readMaybe (unpack parameter)
  case maybeNumber of
    Nothing -> notFound
    Just number -> do
      -- find entry in state
      state <- asks snd
      allEntries <- liftIO (query state QueryEntries)
      let entries = filter
            (\ entry -> isNothing (Entry.deleted entry)) (Map.elems allEntries)
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
                let newEntry = oldEntry
                      { Entry.amount = realToFrac (EntryRequest.amount entryRequest)
                      , Entry.name = EntryRequest.name entryRequest
                      }
                let newEntries = Map.insert
                      (Entry.number oldEntry) newEntry oldEntries
                update state (WriteEntries newEntries)
                return newEntry
              let entryResponse = toResponse entry
              return (json status200 [] entryResponse)

deleteEntry :: Action
deleteEntry = do
  -- get entry number from request parameters
  request <- asks fst
  let parameter = pathInfo request !! 2
  let maybeNumber = readMaybe (unpack parameter)
  case maybeNumber of
    Nothing -> notFound
    Just number -> do
      -- find entry in state
      state <- asks snd
      allEntries <- liftIO (query state QueryEntries)
      let entries = filter
            (\ entry -> isNothing (Entry.deleted entry)) (Map.elems allEntries)
      let maybeEntry = find (\ entry -> Entry.number entry == number) entries
      case maybeEntry of
        Nothing -> notFound
        Just oldEntry -> do
          -- delete the entry
          _ <- liftIO $ do
            oldEntries <- query state QueryEntries
            now <- getCurrentTime
            let newEntry = oldEntry { Entry.deleted = Just now }
            let newEntries = Map.insert
                  (Entry.number oldEntry) newEntry oldEntries
            update state (WriteEntries newEntries)
          return (json status200 [] Null)

findEntries :: ReaderT (a, State) IO [Entry.Entry]
findEntries = do
  state <- asks snd
  allEntries <- liftIO (query state QueryEntries)
  let entries = filter (isNothing . Entry.deleted) (Map.elems allEntries)
  return entries

decodeEntry :: ReaderT (Request, a) IO (Maybe EntryRequest.EntryRequest)
decodeEntry = do
  request <- asks fst
  body <- liftIO (strictRequestBody request)
  let maybeEntryRequest = decode body
  return maybeEntryRequest

createEntry :: EntryRequest.EntryRequest -> ReaderT (a, State) IO Entry.Entry
createEntry entryRequest = do
  state <- asks snd
  liftIO $ do
    oldEntries <- query state QueryEntries
    entry <- toEntry oldEntries entryRequest
    let newEntries = Map.insert (Entry.number entry) entry oldEntries
    update state (WriteEntries newEntries)
    return entry
