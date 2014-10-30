module Ledger.Actions.Entries
  ( getEntries
  ) where

import           Ledger.Internal.Actions (Action, json)
import           Ledger.Models           (QueryEntries (..), entryToResponse)

import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Reader    (asks)
import           Data.Acid               (query)
import           Network.HTTP.Types      (status200)

getEntries :: Action
getEntries = do
  state <- asks snd
  entries <- liftIO (query state QueryEntries)
  let entryResponses = map entryToResponse entries
  return (json status200 [] entryResponses)
