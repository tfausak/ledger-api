module Main where

import Control.Monad (unless)
import Language.Haskell.HLint (hlint)
import System.Exit (exitFailure)

main :: IO ()
main = do
    let arguments =
            [ "lint"
            , "--color"
            , "executable"
            , "library"
            , "test-suite"
            , "Setup.hs"
            ]
    suggestions <- hlint arguments
    unless (null suggestions) exitFailure
