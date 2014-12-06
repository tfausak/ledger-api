{-# LANGUAGE OverloadedStrings #-}

module Ledger.Application.Action.Root where

import Ledger.Application.Action.Common (Action, json)
import Paths_ledger (version)

import Data.Aeson (object, (.=))
import Data.Version (showVersion)
import Network.HTTP.Types (status200)

getRoot :: Action
getRoot = do
    let value = object ["version" .= showVersion version]
    return (json status200 [] value)
