module Ledger.Application
  ( application
  ) where

import           Ledger.Models        (Entry)
import           Ledger.Router        (route)

import           Control.Monad.Reader (runReaderT)
import           Data.Acid            (AcidState)
import           Network.Wai          (Application)

application :: AcidState [Entry] -> Application
application state request respond = do
  let action = route request
  response <- runReaderT action (request, state)
  respond response
