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
        ["jsx.js"] -> static method "static/JSXTransformer-0.12.0.js" "application/javascript"
        ["ledger.css"] -> static method "ledger.css" "text/css"
        ["ledger.jsx"] -> static method "ledger.jsx" "text/jsx"
        ["ledger.manifest"] -> static method "ledger.manifest" "text/cache-manifest"
        ["react.js"] -> static method "static/react-0.12.0.js" "application/javascript"
        ["superagent.js"] -> static method "static/superagent-0.20.0.js" "application/javascript"

        ["apple-touch-icon-precomposed-57.png"] -> static method "static/apple-touch-icon-precomposed-57.png" "image/png"
        ["apple-touch-icon-precomposed-57@2x.png"] -> static method "static/apple-touch-icon-precomposed-57@2x.png" "image/png"
        ["apple-touch-icon-precomposed-60@3x.png"] -> static method "static/apple-touch-icon-precomposed-60@3x.png" "image/png"
        ["apple-touch-icon-precomposed-72.png"] -> static method "static/apple-touch-icon-precomposed-72.png" "image/png"
        ["apple-touch-icon-precomposed-72@2x.png"] -> static method "static/apple-touch-icon-precomposed-72@2x.png" "image/png"
        ["apple-touch-icon-precomposed-76.png"] -> static method "static/apple-touch-icon-precomposed-76.png" "image/png"
        ["apple-touch-icon-precomposed-76@2x.png"] -> static method "static/apple-touch-icon-precomposed-76@2x.png" "image/png"
        ["apple-touch-startup-image-320x460.png"] -> static method "static/apple-touch-startup-image-320x460.png" "image/png"
        ["apple-touch-startup-image-320x460@2x.png"] -> static method "static/apple-touch-startup-image-320x460@2x.png" "image/png"
        ["apple-touch-startup-image-320x548@2x.png"] -> static method "static/apple-touch-startup-image-320x548@2x.png" "image/png"
        ["apple-touch-startup-image-375x647@2x.png"] -> static method "static/apple-touch-startup-image-375x647@2x.png" "image/png"
        ["apple-touch-startup-image-394x736@3x.png"] -> static method "static/apple-touch-startup-image-394x736@3x.png" "image/png"
        ["apple-touch-startup-image-414x716@3x.png"] -> static method "static/apple-touch-startup-image-414x716@3x.png" "image/png"
        ["apple-touch-startup-image-748x1024.png"] -> static method "static/apple-touch-startup-image-748x1024.png" "image/png"
        ["apple-touch-startup-image-748x1024@2x.png"] -> static method "static/apple-touch-startup-image-748x1024@2x.png" "image/png"
        ["apple-touch-startup-image-768x1004.png"] -> static method "static/apple-touch-startup-image-768x1004.png" "image/png"
        ["apple-touch-startup-image-768x1004@2x.png"] -> static method "static/apple-touch-startup-image-768x1004@2x.png" "image/png"
        ["favicon.ico"] -> static method "static/favicon.ico" "image/x-icon"

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
