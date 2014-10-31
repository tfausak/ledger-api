module Ledger.Types
  ( Action
  , State
  ) where

import           Ledger.Models.Entry  (Entry)

import           Control.Monad.Reader (ReaderT)
import           Data.Acid            (AcidState)
import           Network.Wai          (Request, Response)

type Action = ReaderT (Request, State) IO Response

type State = AcidState [Entry]
