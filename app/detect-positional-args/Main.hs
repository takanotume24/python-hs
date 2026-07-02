module Main (main) where

import PythonHS.Structure.DetectPositionalArgs (detectPositionalArgs)
import PythonHS.Structure.DetectPositionalArgsFromDirectory (detectPositionalArgsFromDirectory)
import PythonHS.Structure.FormatViolationsJson (formatViolationsJson)
import System.Directory (doesDirectoryExist)
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
      isDir <- doesDirectoryExist path
      violations <-
        if isDir
          then detectPositionalArgsFromDirectory path
          else detectPositionalArgs path
      putStrLn (formatViolationsJson violations)
      if null violations
        then pure ()
        else exitFailure
