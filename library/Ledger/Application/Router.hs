{-# LANGUAGE OverloadedStrings #-}

module Ledger.Application.Router where

import Ledger.Application.Action

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Network.HTTP.Types (Method)
import Network.Wai (Request, pathInfo, requestMethod)

route :: Request -> Action
route request =
    let path = pathInfo request
        method = requestMethod request
    in  maybe notFoundA (fromMaybe notAllowedA) (route' path method)

route' :: [Text] -> Method -> Maybe (Maybe Action)
route' path method = case path of
    [] -> Just $ case method of
        "GET" -> Just getRootA
        _ -> Nothing

    ["entries"] -> Just $ case method of
        "GET" -> Just getEntriesA
        "POST" -> Just postEntriesA
        _ -> Nothing

    ["entries", eid] -> Just $ case method of
        "GET" -> Just (getEntryA eid)
        "PUT" -> Just (putEntryA eid)
        "DELETE" -> Just (deleteEntryA eid)
        _ -> Nothing

    ["keys"] -> Just $ case method of
        "POST" -> Just postKeysA
        _ -> Nothing

    ["keys", kid] -> Just $ case method of
        "GET" -> Just (getKeyA kid)
        "DELETE" -> Just (deleteKeyA kid)
        _ -> Nothing

    _ -> Nothing
