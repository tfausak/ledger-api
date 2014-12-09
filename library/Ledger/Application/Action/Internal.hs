module Ledger.Application.Action.Internal where

import Ledger.Application.State (State)

import Control.Monad.Reader (ReaderT, asks)
import Data.Acid (AcidState)
import Network.Wai (Request)

getRequest :: (Monad m) => ReaderT (Request, a) m Request
getRequest = asks fst

getState :: (Monad m) => ReaderT (a, AcidState State) m (AcidState State)
getState = asks snd
