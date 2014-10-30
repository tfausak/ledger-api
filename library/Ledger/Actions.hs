{-# LANGUAGE OverloadedStrings #-}

module Ledger.Actions
  ( notFound
  ) where

import           Ledger.Internal.Actions (Action, json)

import           Data.Aeson              (Value (Null))
import           Network.HTTP.Types      (status404)

notFound :: Action
notFound = return (json status404 [] Null)
