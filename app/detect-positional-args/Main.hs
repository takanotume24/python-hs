module Main (main) where

import PythonHS.Structure.DetectPositionalArgs (detectPositionalArgs)
import PythonHS.Structure.DetectPositionalArgsFromDirectory (detectPositionalArgsFromDirectory)
import PythonHS.Structure.FormatViolationsJson (formatViolationsJson)
import PythonHS.Structure.FormatViolationsPlain (formatViolationsPlain)
import PythonHS.Structure.PositionalArgViolation (PositionalArgViolation)
import System.Directory (doesDirectoryExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  let run formatter path = do
        isDir <- doesDirectoryExist path
        violations <-
          if isDir
            then detectPositionalArgsFromDirectory path
            else detectPositionalArgs path
        putStrLn (formatter violations)
        if null violations
          then pure ()
          else exitFailure
   in case args of
        ["--json", path] -> run formatViolationsJson path
        [path] -> run formatViolationsPlain path
        _ -> do
          putStrLn "Usage: detect-positional-args [--json] <path>"
          exitFailure
