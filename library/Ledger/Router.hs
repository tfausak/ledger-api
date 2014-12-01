{-# LANGUAGE OverloadedStrings #-}

module Ledger.Router where

import           Ledger.Actions (Action)
import qualified Ledger.Actions as Actions

import           Network.Wai    (Request, pathInfo, requestMethod)

route :: Request -> Action
route request =
  let path = pathInfo request
      method = requestMethod request
  in  case path of
        ["api", "entries"] -> case method of
          "GET" -> Actions.getEntries
          "POST" -> Actions.postEntries
          _ -> Actions.notAllowed

        ["api", "entries", entryId] -> case method of
          "GET" -> Actions.getEntry entryId
          "PUT" -> Actions.putEntry entryId
          "DELETE" -> Actions.deleteEntry entryId
          _ -> Actions.notAllowed

        _ -> Actions.notFound
