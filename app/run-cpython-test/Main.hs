module Main (main) where

import Control.Monad (unless)
import Data.Maybe (isJust)
import PythonHS.CPython.AllCasesPassed (allCasesPassed)
import PythonHS.CPython.CaseReport (CaseReport (..))
import PythonHS.CPython.ReportLines (reportLines)
import PythonHS.CPython.RunCpythonTest (runCpythonTest)
import PythonHS.CPython.RunCpythonTestConfig (RunCpythonTestConfig (..))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right config -> do
      result <- runCpythonTest config
      case result of
        Left err -> do
          hPutStrLn stderr ("run-cpython-test: " ++ err)
          exitFailure
        Right reports -> do
          mapM_ putStrLn (reportLines reports)
          let failedCount = length (filter (isJust . caseReportFailure) reports)
          putStrLn ("summary: " ++ show (length reports) ++ " cases, " ++ show failedCount ++ " failed")
          unless (allCasesPassed reports) exitFailure
  where
    parseArgs = go RunCpythonTestConfig {runCpythonTestConfigRepoRoot = ".", runCpythonTestConfigAllowlistPath = "cpython-tests.allowlist"}
      where
        go config [] = Right config
        go config ("--repo-root" : value : rest) =
          go config {runCpythonTestConfigRepoRoot = value} rest
        go config ("--allowlist" : value : rest) =
          go config {runCpythonTestConfigAllowlistPath = value} rest
        go _ (unknown : _) = Left ("unknown argument: " ++ unknown)
