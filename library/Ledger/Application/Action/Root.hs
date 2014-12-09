{-# LANGUAGE OverloadedStrings #-}

module Ledger.Application.Action.Root where

import Ledger.Application.Action.Common
import Paths_ledger (version)

import Data.Aeson (object, (.=))
import Data.Version (showVersion)
import qualified Network.HTTP.Types as HTTP

getRootA :: Action
getRootA = do
    let value = object
            [ "version" .= showVersion version
            ]
    json HTTP.status200 value
