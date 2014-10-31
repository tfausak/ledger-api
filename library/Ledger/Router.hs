{-# LANGUAGE OverloadedStrings #-}

module Ledger.Router
  ( route
  , static
  ) where

import           Ledger.Actions         (Action)
import qualified Ledger.Actions         as Actions
import           Ledger.Utilities       (file)

import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString        (ByteString)
import           Network.HTTP.Types     (Method)
import           Network.Wai            (Request, pathInfo, requestMethod)

route :: Request -> Action
route request =
  let path = pathInfo request
      method = requestMethod request
  in  case path of
        [] -> static method "ledger.html" "text/html"
        ["jsx.js"] -> static method "JSXTransformer-0.12.0.js" "application/javascript"
        ["ledger.jsx"] -> static method "ledger.jsx" "text/jsx"
        ["react.js"] -> static method "react-0.12.0.js" "application/javascript"
        ["superagent.js"] -> static method "superagent-0.20.0.js" "application/javascript"

        ["entries"] -> case method of
          "GET" -> Actions.getEntries
          "POST" -> Actions.postEntries
          _ -> Actions.notAllowed
        ["entries", _] -> case method of
          "GET" -> Actions.getEntry
          "PUT" -> Actions.putEntry
          "DELETE" -> Actions.deleteEntry
          _ -> Actions.notAllowed

        _ -> Actions.notFound

static :: Method -> FilePath -> ByteString -> Action
static method path contentType = case method of
  "GET" -> liftIO (file path contentType)
  _ -> Actions.notAllowed
