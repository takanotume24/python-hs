module Main (main) where

import PythonHS.Structure.DetectPositionalArgs (detectPositionalArgs)
import PythonHS.Structure.DetectPositionalArgsFromDirectory (detectPositionalArgsFromDirectory)
import PythonHS.Structure.DetectPositionalArgsFromDirectoryConfig (DetectPositionalArgsFromDirectoryConfig (..))
import PythonHS.Structure.FormatViolationsJson (formatViolationsJson)
import PythonHS.Structure.FormatViolationsPlain (formatViolationsPlain)
import PythonHS.Structure.PositionalArgViolation (PositionalArgViolation)
import System.Directory (doesDirectoryExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Just (excludes, path, isJson) ->
      let formatter = if isJson then formatViolationsJson else formatViolationsPlain
       in do
            isDir <- doesDirectoryExist path
            violations <-
              if isDir
                then detectPositionalArgsFromDirectory (DetectPositionalArgsFromDirectoryConfig path excludes)
                else detectPositionalArgs path
            putStrLn (formatter violations)
            if null violations
              then pure ()
              else exitFailure
    Nothing -> do
      putStrLn "Usage: detect-positional-args [--json] [--exclude PATTERN]... <path>"
      exitFailure
  where
    parseArgs [] = Just ([], "", False)
    parseArgs [path] = Just ([], path, False)
    parseArgs ("--json" : rest) = fmap (\(e, p, _) -> (e, p, True)) (parseArgs rest)
    parseArgs ("--exclude" : _ : []) = Nothing
    parseArgs ("--exclude" : pat : rest) = fmap (\(e, p, j) -> (pat : e, p, j)) (parseArgs rest)
    parseArgs _ = Nothing
