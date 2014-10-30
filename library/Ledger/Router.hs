module Ledger.Router
  ( route
  , routes
  ) where

import           Ledger.Actions          (notAllowed, notFound)
import           Ledger.Internal.Actions (Action)

import           Data.Map                (Map, findWithDefault, fromList,
                                          lookup)
import           Data.Text               (Text)
import           Network.HTTP.Types      (Method)
import           Network.Wai             (Request, pathInfo, requestMethod)
import           Prelude                 hiding (lookup, null)

route :: Request -> Action
route request = case lookup (pathInfo request) routes of
  Nothing -> notFound
  Just methods -> findWithDefault notAllowed (requestMethod request) methods

routes :: Map [Text] (Map Method Action)
routes = fromList
  [
  ]
