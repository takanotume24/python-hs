module Main (main) where

import Control.Monad (unless)
import Data.Maybe (isJust)
import PythonHS.CPython.AllCasesPassed (allCasesPassed)
import PythonHS.CPython.CaseReport (CaseReport (..))
import PythonHS.CPython.FailureCategorySummaryLines (failureCategorySummaryLines)
import PythonHS.CPython.FormatCaseId (formatCaseId)
import PythonHS.CPython.LoadCaseIds (loadCaseIds)
import PythonHS.CPython.ReportJson (reportJson)
import PythonHS.CPython.ReportLines (reportLines)
import PythonHS.CPython.RunCpythonTest (runCpythonTest)
import PythonHS.CPython.RunCpythonTestConfig (RunCpythonTestConfig (..))
import PythonHS.CPython.RunCpythonTestOptions (RunCpythonTestOptions (..))
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
    Right options
      | runCpythonTestOptionsList options -> do
          let config = runCpythonTestOptionsConfig options
          caseIdsResult <- loadCaseIds (runCpythonTestConfigRepoRoot config) (runCpythonTestConfigAllowlistPath config)
          case caseIdsResult of
            Left err -> do
              hPutStrLn stderr ("run-cpython-test: " ++ err)
              exitFailure
            Right caseIds -> mapM_ (putStrLn . formatCaseId) caseIds
      | otherwise -> do
          result <- runCpythonTest (runCpythonTestOptionsConfig options)
          case result of
            Left err -> do
              hPutStrLn stderr ("run-cpython-test: " ++ err)
              exitFailure
            Right reports -> do
              if runCpythonTestOptionsJson options
                then putStr (reportJson reports)
                else do
                  mapM_ putStrLn (reportLines reports)
                  mapM_ putStrLn (failureCategorySummaryLines reports)
                  let failedCount = length (filter (isJust . caseReportFailure) reports)
                  putStrLn ("summary: " ++ show (length reports) ++ " cases, " ++ show failedCount ++ " failed")
              unless (allCasesPassed reports) exitFailure
  where
    parseArgs = go initialOptions
      where
        initialOptions =
          RunCpythonTestOptions
            { runCpythonTestOptionsConfig =
                RunCpythonTestConfig
                  { runCpythonTestConfigRepoRoot = ".",
                    runCpythonTestConfigAllowlistPath = "cpython-tests.allowlist",
                    runCpythonTestConfigFileMode = False
                  },
              runCpythonTestOptionsJson = False,
              runCpythonTestOptionsList = False
            }

        go options [] = Right options
        go options ("--json" : rest) =
          go options {runCpythonTestOptionsJson = True} rest
        go options ("--list" : rest) =
          go options {runCpythonTestOptionsList = True} rest
        go options ("--file" : rest) =
          go
            options
              { runCpythonTestOptionsConfig =
                  (runCpythonTestOptionsConfig options) {runCpythonTestConfigFileMode = True}
              }
            rest
        go options ("--repo-root" : value : rest) =
          go
            options
              { runCpythonTestOptionsConfig =
                  (runCpythonTestOptionsConfig options) {runCpythonTestConfigRepoRoot = value}
              }
            rest
        go options ("--allowlist" : value : rest) =
          go
            options
              { runCpythonTestOptionsConfig =
                  (runCpythonTestOptionsConfig options) {runCpythonTestConfigAllowlistPath = value}
              }
            rest
        go _ (unknown : _) = Left ("unknown argument: " ++ unknown)
