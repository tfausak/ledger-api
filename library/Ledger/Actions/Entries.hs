module Ledger.Actions.Entries
  ( getEntries
  ) where

import           Ledger.Internal.Actions (Action, json)

import           Network.HTTP.Types      (status200)

getEntries :: Action
getEntries = do
  let entries = [] :: [Bool] -- TODO
  return (json status200 [] entries)
