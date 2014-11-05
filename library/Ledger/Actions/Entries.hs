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
      respondWithEntry entry

getEntry :: Action
getEntry = do
  maybeNumber <- getEntryNumber
  maybeEntry <- findEntry maybeNumber
  case maybeEntry of
    Nothing -> notFound
    Just entry -> respondWithEntry entry

putEntry :: Action
putEntry = do
  maybeNumber <- getEntryNumber
  maybeEntry <- findEntry maybeNumber
  case maybeEntry of
    Nothing -> notFound
    Just oldEntry -> do
      maybeEntryRequest <- decodeEntry
      case maybeEntryRequest of
        Nothing -> badRequest
        Just entryRequest -> do
          newEntry <- updateEntry oldEntry entryRequest
          respondWithEntry newEntry

deleteEntry :: Action
deleteEntry = do
  maybeNumber <- getEntryNumber
  maybeEntry <- findEntry maybeNumber
  case maybeEntry of
    Nothing -> notFound
    Just entry -> do
      destroyEntry entry
      let response = json status200 [] Null
      return response

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

respondWithEntry :: Entry.Entry -> Action
respondWithEntry entry = do
  let entryResponse = toResponse entry
  let response = json status200 [] entryResponse
  return response

getEntryNumber :: ReaderT (Request, a) IO (Maybe Integer)
getEntryNumber = do
  request <- asks fst
  let parameter = pathInfo request !! 2
  let maybeNumber = readMaybe (unpack parameter)
  return maybeNumber

findEntry :: Maybe Integer -> ReaderT (a, State) IO (Maybe Entry.Entry)
findEntry maybeNumber = case maybeNumber of
  Nothing -> return Nothing
  Just number -> do
    entries <- findEntries
    let maybeEntry = find ((number ==) . Entry.number) entries
    return maybeEntry

updateEntry :: Entry.Entry -> EntryRequest.EntryRequest -> ReaderT (a, State) IO Entry.Entry
updateEntry entry entryRequest = do
  state <- asks snd
  liftIO $ do
    oldEntries <- query state QueryEntries
    let newEntry = entry
          { Entry.amount = realToFrac (EntryRequest.amount entryRequest)
          , Entry.name = EntryRequest.name entryRequest
          }
    let newEntries = Map.insert (Entry.number entry) newEntry oldEntries
    update state (WriteEntries newEntries)
    return newEntry

destroyEntry :: Entry.Entry -> ReaderT (a, State) IO ()
destroyEntry entry = do
  state <- asks snd
  liftIO $ do
    oldEntries <- query state QueryEntries
    now <- getCurrentTime
    let newEntry = entry { Entry.deleted = Just now }
    let newEntries = Map.insert (Entry.number entry) newEntry oldEntries
    update state (WriteEntries newEntries)
    return ()
