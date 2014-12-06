{-# LANGUAGE OverloadedStrings #-}

module Ledger.Application.Router where

import qualified Ledger.Application.Action as Action

import Network.Wai (Request, pathInfo, requestMethod)

route :: Request -> Action.Action
route request =
    let path = pathInfo request
        method = requestMethod request
    in  case path of
            [] -> case method of
                "GET" -> Action.getRoot
                _ -> Action.notAllowed

            ["entries"] -> case method of
                "GET" -> Action.getEntries
                "POST" -> Action.postEntries
                _ -> Action.notAllowed
            ["entries", entryId] -> case method of
                "GET" -> Action.getEntry entryId
                "PUT" -> Action.putEntry entryId
                "DELETE" -> Action.deleteEntry entryId
                _ -> Action.notAllowed

            ["keys"] -> case method of
                "POST" -> Action.postKeys
                _ -> Action.notAllowed
            ["keys", keyId] -> case method of
                "GET" -> Action.getKey keyId
                "DELETE" -> Action.deleteKey keyId
                _ -> Action.notAllowed

            _ -> Action.notFound
