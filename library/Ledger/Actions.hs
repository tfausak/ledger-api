{-# LANGUAGE OverloadedStrings #-}

module Ledger.Actions
  ( notFound
  ) where

import           Control.Monad.Reader (Reader)
import           Network.HTTP.Types   (status404)
import           Network.Wai          (Request, Response, responseLBS)

notFound :: Reader Request Response
notFound = return (responseLBS status404 [] "")
