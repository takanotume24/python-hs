module Main (main) where

import HsLint.CLI (runCheckCommand, runPositionalCommand)
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("check" : rest) -> runCheckCommand rest
    ("positional" : rest) -> runPositionalCommand rest
    _ -> do
      putStrLn "Usage: structure-lint (check|positional) [OPTIONS]"
      exitFailure
