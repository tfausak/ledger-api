module Ledger.State
  ( State
  ) where

import           Ledger.Models.Entry (Entries)

import           Data.Acid           (AcidState)

type State = AcidState Entries
