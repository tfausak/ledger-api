module Ledger.Router
  ( route
  ) where

import           Ledger.Actions       (notFound)

import           Control.Monad.Reader (Reader)
import           Network.Wai          (Request, Response, pathInfo)

route :: Request -> Reader Request Response
route request = case pathInfo request of
  _ -> notFound
