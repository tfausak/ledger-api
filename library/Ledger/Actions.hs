{-# LANGUAGE OverloadedStrings #-}

module Ledger.Actions
  ( notFound
  ) where

import           Ledger.Internal.Actions (json)

import           Control.Monad.Reader    (Reader)
import           Data.Aeson              (Value (Null))
import           Network.HTTP.Types      (status404)
import           Network.Wai             (Request, Response)

notFound :: Reader Request Response
notFound = return (json status404 [] Null)
