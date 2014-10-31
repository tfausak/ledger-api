{-# LANGUAGE OverloadedStrings #-}

module Ledger.Router
  ( route
  ) where

import qualified Ledger.Actions          as Actions
import qualified Ledger.Internal.Actions as Actions

import           Network.Wai             (Request, pathInfo, requestMethod)
import           Prelude                 hiding (lookup, null)

route :: Request -> Actions.Action
route request = case pathInfo request of
  ["entries"] -> case requestMethod request of
    "GET" -> Actions.getEntries
    "POST" -> Actions.postEntries
    _ -> Actions.notAllowed
  ["entries", _] -> case requestMethod request of
    "GET" -> Actions.getEntry
    "PUT" -> Actions.putEntry
    "DELETE" -> Actions.deleteEntry
    _ -> Actions.notAllowed
  _ -> Actions.notFound
