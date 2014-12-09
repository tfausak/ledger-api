{-# LANGUAGE OverloadedStrings #-}

module Ledger.Application.Router where

import Ledger.Application.Action

import Network.Wai (Request, pathInfo, requestMethod)

route :: Request -> Action
route request =
    let path = pathInfo request
        method = requestMethod request
    in  case path of
            [] -> case method of
                "GET" -> getRootA
                _ -> notAllowedA

            ["entries"] -> case method of
                "GET" -> getEntriesA
                "POST" -> postEntriesA
                _ -> notAllowedA
            ["entries", eid] -> case method of
                "GET" -> getEntryA eid
                "PUT" -> putEntryA eid
                "DELETE" -> deleteEntryA eid
                _ -> notAllowedA

            ["keys"] -> case method of
                "POST" -> postKeysA
                _ -> notAllowedA
            ["keys", kid] -> case method of
                "GET" -> getKeyA kid
                "DELETE" -> deleteKeyA kid
                _ -> notAllowedA

            _ -> notFoundA
