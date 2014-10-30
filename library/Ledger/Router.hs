module Ledger.Router
  ( route
  , routes
  ) where

import           Ledger.Actions          (notFound)
import           Ledger.Internal.Actions (Action)

import           Data.Map                (Map, empty, findWithDefault, fromList)
import           Data.Text               (Text)
import           Network.HTTP.Types      (Method)
import           Network.Wai             (Request, pathInfo, requestMethod)

route :: Request -> Action
route request =
  let methods = findWithDefault empty (pathInfo request) routes
  in  findWithDefault notFound (requestMethod request) methods

routes :: Map [Text] (Map Method Action)
routes = fromList
  [
  ]
