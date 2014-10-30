module Ledger.Main
  ( main
  ) where

import           Ledger.Application       (application)

import           Network.Wai.Handler.Warp (run)

main :: IO ()
main = run 8080 application
