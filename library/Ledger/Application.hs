module Ledger.Application
  ( application
  ) where

import           Ledger.Internal.Main (State)
import           Ledger.Router        (route)

import           Control.Monad.Reader (runReaderT)
import           Network.Wai          (Application)

application :: State -> Application
application state request respond = do
  let action = route request
  response <- runReaderT action (request, state)
  respond response
