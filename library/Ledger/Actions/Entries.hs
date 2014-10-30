module Ledger.Actions.Entries
  ( getEntries
  ) where

import           Ledger.Internal.Actions (Action, json)
import           Ledger.Models           (Entry)

import           Network.HTTP.Types      (status200)

getEntries :: Action
getEntries = do
  let entries = [] :: [Entry]
  return (json status200 [] entries)
