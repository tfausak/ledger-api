module Ledger.Application
  ( application
  , middleware
  ) where

import           Ledger.Router                        (route)
import           Ledger.State                         (State)

import           Control.Monad.Reader                 (runReaderT)
import           Network.Wai                          (Application)
import           Network.Wai.Middleware.Gzip          (def, gzip)
import           Network.Wai.Middleware.RequestLogger (logStdout)

application :: State -> Application
application state = middleware $ \ request respond -> do
    let action = route request
    response <- runReaderT action (request, state)
    respond response

middleware :: Application -> Application
middleware = gzip def . logStdout
