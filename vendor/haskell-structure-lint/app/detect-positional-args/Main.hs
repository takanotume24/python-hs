module Main (main) where

import HsLint.CLI (runPositionalCommand)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= runPositionalCommand
