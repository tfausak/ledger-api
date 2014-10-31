module Ledger.Types
  ( State
  ) where

import           Ledger.Models.Entry (Entry)

import           Data.Acid           (AcidState)

type State = AcidState [Entry]
