module Main where

import Control.Monad (unless)
import Language.Haskell.HLint (hlint)
import System.Exit (exitFailure)

main :: IO ()
main = do
    let arguments = ["--color", "executable", "library", "test-suite"]
    suggestions <- hlint arguments
    unless (null suggestions) exitFailure
