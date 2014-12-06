module Ledger.Application.Application where

import Ledger.Application.Router (route)
import Ledger.Application.State (State)

import Control.Monad.Reader (runReaderT)
import Data.Acid (AcidState)
import Network.Wai (Application)

application :: AcidState State -> Application
application state request respond = do
    let action = route request
    response <- runReaderT action (request, state)
    respond response
