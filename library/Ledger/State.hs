module Ledger.State where

import           Ledger.Models.Entry (Entries)

import           Data.Acid           (AcidState)
import           Data.Map            (fromList)

type State = AcidState Entries

defaultState :: Entries
defaultState = fromList []
