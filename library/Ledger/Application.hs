{-# LANGUAGE OverloadedStrings #-}

module Ledger.Application
  ( application
  , middleware
  ) where

import           Ledger.Router                        (route)
import           Ledger.State                         (State)

import           Control.Monad.Reader                 (runReaderT)
import           Network.Wai                          (Application, Middleware)
import           Network.Wai.Middleware.Gzip          (def, gzip)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Prelude                              hiding (lookup)

application :: State -> Application
application state = middleware $ \ request respond -> do
    let action = route request
    response <- runReaderT action (request, state)
    respond response

middleware :: Middleware
middleware = gzip def . logStdout
