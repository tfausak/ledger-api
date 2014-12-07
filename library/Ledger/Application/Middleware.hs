{-# LANGUAGE OverloadedStrings #-}

module Ledger.Application.Middleware (middleware) where

import Network.HTTP.Types (hContentType, renderStdMethod)
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors (CorsResourcePolicy, cors, corsMethods,
                                    corsRequestHeaders,
                                    simpleCorsResourcePolicy)
import Network.Wai.Middleware.Gzip (def, gzip)
import Network.Wai.Middleware.RequestLogger (logStdout)

middleware :: Middleware
middleware
    = gzip def
    . cors (const (Just policy))
    . logStdout

policy :: CorsResourcePolicy
policy = simpleCorsResourcePolicy
    { corsMethods = map renderStdMethod [minBound .. maxBound]
    , corsRequestHeaders = [hContentType, "Origin"]
    }
