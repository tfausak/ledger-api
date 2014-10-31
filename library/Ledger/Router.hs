{-# LANGUAGE OverloadedStrings #-}

module Ledger.Router
  ( route
  ) where

import qualified Ledger.Actions as Actions
import           Ledger.Types   (Action)

import           Network.Wai    (Request, pathInfo, requestMethod)

route :: Request -> Action
route request =
  let path = pathInfo request
      method = requestMethod request
  in  case path of
        ["entries"] -> case method of
          "GET" -> Actions.getEntries
          "POST" -> Actions.postEntries
          _ -> Actions.notAllowed
        ["entries", _] -> case method of
          "GET" -> Actions.getEntry
          "PUT" -> Actions.putEntry
          "DELETE" -> Actions.deleteEntry
          _ -> Actions.notAllowed
        _ -> Actions.notFound
