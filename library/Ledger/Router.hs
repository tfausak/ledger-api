{-# LANGUAGE OverloadedStrings #-}

module Ledger.Router
  ( route
  ) where

import qualified Ledger.Actions          as Actions
import           Ledger.Internal.Actions (Action)

import           Network.Wai             (Request, pathInfo, requestMethod)
import           Prelude                 hiding (lookup, null)

route :: Request -> Action
route request = case pathInfo request of
  ["entries"] -> case requestMethod request of
    "GET" -> Actions.getEntries
    "POST" -> Actions.postEntries
    _ -> Actions.notAllowed
  ["entries", _] -> case requestMethod request of
    "GET" -> Actions.getEntry
    _ -> Actions.notAllowed
  _ -> Actions.notFound
