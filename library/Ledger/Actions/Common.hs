{-# LANGUAGE OverloadedStrings #-}

module Ledger.Actions.Common
  ( Action
  , badRequest
  , notFound
  , notAllowed
  ) where

import           Ledger.State         (State)
import           Ledger.Utilities     (json)

import           Control.Monad.Reader (ReaderT)
import           Data.Aeson           (Value (Null))
import           Network.HTTP.Types   (status400, status404, status405)
import           Network.Wai          (Request, Response)

type Action = ReaderT (Request, State) IO Response

badRequest :: Action
badRequest = return (json status400 [] Null)

notFound :: Action
notFound = return (json status404 [] Null)

notAllowed :: Action
notAllowed = return (json status405 [] Null)
