module Main (main) where

import PythonHS.Structure.DetectPositionalArgs (detectPositionalArgs)
import PythonHS.Structure.FormatViolationsJson (formatViolationsJson)
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "Usage: detect-positional-args <path>"
      exitFailure
    (path : _) -> do
      violations <- detectPositionalArgs path
      putStrLn (formatViolationsJson violations)
      if null violations
        then pure ()
        else exitFailure
