{-# LANGUAGE OverloadedStrings #-}

module Ledger.Actions.Common
  ( badRequest
  , notFound
  , notAllowed
  ) where

import           Ledger.Types       (Action)
import           Ledger.Utilities   (json)

import           Data.Aeson         (Value (Null))
import           Network.HTTP.Types (status400, status404, status405)

badRequest :: Action
badRequest = return (json status400 [] Null)

notFound :: Action
notFound = return (json status404 [] Null)

notAllowed :: Action
notAllowed = return (json status405 [] Null)
