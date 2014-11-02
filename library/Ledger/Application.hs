{-# LANGUAGE OverloadedStrings #-}

module Ledger.Application
  ( application
  , middleware
  ) where

import           Ledger.Router                        (route)
import           Ledger.State                         (State)

import           Control.Monad.Reader                 (runReaderT)
import           Data.ByteString                      (ByteString)
import           Data.Configurator                    (lookup)
import           Data.Configurator.Types              (Config)
import           Network.Wai                          (Application, Middleware)
import           Network.Wai.Middleware.Gzip          (def, gzip)
import           Network.Wai.Middleware.HttpAuth      (basicAuth)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Prelude                              hiding (lookup)

application :: Config -> State -> Application
application config state = middleware config $ \ request respond -> do
    let action = route request
    response <- runReaderT action (request, state)
    respond response

middleware :: Config -> Middleware
middleware config
  = basicAuth (authenticate config) "Ledger"
  . gzip def
  . logStdout

authenticate :: Config -> ByteString -> ByteString -> IO Bool
authenticate config username password = do
  maybeUsername <- lookup config "ledger.username"
  let usernameMatches = case maybeUsername of
        Nothing -> True
        Just expectedUsername -> username == expectedUsername

  maybePassword <- lookup config "ledger.password"
  let passwordMatches = case maybePassword of
        Nothing -> True
        Just expectedPassword -> password == expectedPassword

  return (usernameMatches && passwordMatches)
