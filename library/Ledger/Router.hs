module Ledger.Router
  ( route
  ) where

import           Ledger.Actions          (notFound)
import           Ledger.Internal.Actions (Action)

import           Network.Wai             (Request, pathInfo)

route :: Request -> Action
route request = case pathInfo request of
  _ -> notFound
