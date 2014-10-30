module Ledger.Application
  ( application
  ) where

import           Ledger.Router        (route)

import           Control.Monad.Reader (runReader)
import           Network.Wai          (Application)

application :: Application
application request respond = do
  let action = route request
  let response = runReader action request
  respond response
