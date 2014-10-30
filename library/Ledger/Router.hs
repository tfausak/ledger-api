{-# LANGUAGE OverloadedStrings #-}

module Ledger.Router
  ( route
  ) where

import           Control.Monad.Reader (Reader)
import           Network.HTTP.Types   (status200)
import           Network.Wai          (Request, Response, pathInfo, responseLBS)

route :: Request -> Reader Request Response
route request = case pathInfo request of
  _ -> return (responseLBS status200 [] "")
