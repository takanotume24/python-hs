module Main (main) where

import HsLint.CLI (runCheckCommand)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= runCheckCommand
