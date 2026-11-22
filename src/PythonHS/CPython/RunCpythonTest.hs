module PythonHS.CPython.RunCpythonTest (runCpythonTest) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd, isPrefixOf)
import PythonHS.CPython.BuildDriverSource (buildDriverSource)
import PythonHS.CPython.CaseId (CaseId (..))
import PythonHS.CPython.CaseReport (CaseReport (..))
import PythonHS.CPython.ParseCaseId (parseCaseId)
import PythonHS.CPython.ReadFileSafely (readFileSafely)
import PythonHS.CPython.RunCpythonTestConfig (RunCpythonTestConfig (..))
import PythonHS.RunSourceVmWithSearchPaths (runSourceVmWithSearchPaths)
import PythonHS.RunSourceVmWithSearchPathsConfig (RunSourceVmWithSearchPathsConfig (..))
import System.FilePath ((</>))

-- | Run every allowlisted CPython test method and collect the outcomes.
runCpythonTest :: RunCpythonTestConfig -> IO (Either String [CaseReport])
runCpythonTest config = do
  allowlistResult <- readFileSafely allowlistPath
  case allowlistResult of
    Left err -> pure (Left err)
    Right contents ->
      case traverse parseEntry (filter isEntry (lines contents)) of
        Left err -> pure (Left err)
        Right caseIds -> Right <$> mapM runCase caseIds
  where
    repoRoot = runCpythonTestConfigRepoRoot config
    allowlistPath = runCpythonTestConfigAllowlistPath config
    cpythonLibPath = repoRoot </> "third_party/cpython/Lib"
    shimPath = repoRoot </> "cpython-shim"
    marker = "__PYTHON_HS_CPYTHON_TEST_OK__"

    isEntry line =
      let trimmed = trim line
       in not (null trimmed) && not ("#" `isPrefixOf` trimmed)

    trim = dropWhile isSpace . dropWhileEnd isSpace

    parseEntry = parseCaseId

    runCase caseId = do
      sourceResult <- readFileSafely (repoRoot </> caseIdFile caseId)
      case sourceResult of
        Left err -> pure (failureReport caseId err)
        Right source ->
          case buildDriverSource source caseId of
            Left err -> pure (failureReport caseId err)
            Right driver -> do
              outputsResult <-
                runSourceVmWithSearchPaths
                  RunSourceVmWithSearchPathsConfig
                    { runSourceVmWithSearchPathsSearchPaths = [shimPath, cpythonLibPath],
                      runSourceVmWithSearchPathsSource = driver
                    }
              pure (CaseReport {caseReportId = caseId, caseReportFailure = failureFromOutputs outputsResult})

    failureFromOutputs (Left err) = Just err
    failureFromOutputs (Right outputs)
      | marker `elem` outputs = Nothing
      | otherwise = Just ("missing success marker, output: " ++ unwords outputs)

    failureReport caseId err = CaseReport {caseReportId = caseId, caseReportFailure = Just err}
