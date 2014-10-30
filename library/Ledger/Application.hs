{-# LANGUAGE OverloadedStrings #-}

module Ledger.Application
  ( application
  ) where

import           Network.HTTP.Types (status200)
import           Network.Wai        (Application, responseLBS)

application :: Application
application _request respond = respond (responseLBS status200 [] "")
