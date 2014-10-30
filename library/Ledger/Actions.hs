{-# LANGUAGE OverloadedStrings #-}

module Ledger.Actions
  ( notAllowed
  , notFound
  ) where

import           Ledger.Internal.Actions (Action, json)

import           Data.Aeson              (Value (Null))
import           Network.HTTP.Types      (status404, status405)

notAllowed :: Action
notAllowed = return (json status405 [] Null)

notFound :: Action
notFound = return (json status404 [] Null)
