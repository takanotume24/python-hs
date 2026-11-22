module PythonHS.CPython.RunCpythonTest (runCpythonTest) where

import PythonHS.CPython.BuildDriverSource (buildDriverSource)
import PythonHS.CPython.BuildFileDriverSource (buildFileDriverSource)
import PythonHS.CPython.CaseFailure (CaseFailure (..))
import PythonHS.CPython.CaseId (CaseId (..))
import PythonHS.CPython.CaseReport (CaseReport (..))
import PythonHS.CPython.ClassifyCaseFailure (classifyCaseFailure)
import PythonHS.CPython.LoadCaseIds (loadCaseIds)
import PythonHS.CPython.ReadFileSafely (readFileSafely)
import PythonHS.CPython.RunCpythonTestConfig (RunCpythonTestConfig (..))
import PythonHS.RunSourceVmWithSearchPaths (runSourceVmWithSearchPaths)
import PythonHS.RunSourceVmWithSearchPathsConfig (RunSourceVmWithSearchPathsConfig (..))
import System.FilePath ((</>))

-- | Run every allowlisted CPython test method and collect the outcomes.
runCpythonTest :: RunCpythonTestConfig -> IO (Either String [CaseReport])
runCpythonTest config = do
  caseIdsResult <- loadCaseIds repoRoot (runCpythonTestConfigAllowlistPath config)
  case caseIdsResult of
    Left err -> pure (Left err)
    Right caseIds -> Right <$> mapM runCase caseIds
  where
    repoRoot = runCpythonTestConfigRepoRoot config
    cpythonLibPath = repoRoot </> "third_party/cpython/Lib"
    shimPath = repoRoot </> "cpython-shim"
    marker = "__PYTHON_HS_CPYTHON_TEST_OK__"

    buildDriver
      | runCpythonTestConfigFileMode config = buildFileDriverSource
      | otherwise = buildDriverSource

    runCase caseId = do
      sourceResult <- readFileSafely (repoRoot </> caseIdFile caseId)
      case sourceResult of
        Left err -> pure (failureReport caseId err)
        Right source ->
          case buildDriver source caseId of
            Left err -> pure (failureReport caseId err)
            Right driver -> do
              outputsResult <-
                runSourceVmWithSearchPaths
                  RunSourceVmWithSearchPathsConfig
                    { runSourceVmWithSearchPathsSearchPaths = [shimPath, cpythonLibPath],
                      runSourceVmWithSearchPathsSource = driver
                    }
              pure (CaseReport {caseReportId = caseId, caseReportFailure = failureFromOutputs outputsResult})

    failureFromOutputs (Left err) = Just (makeFailure err)
    failureFromOutputs (Right outputs)
      | marker `elem` outputs = Nothing
      | otherwise = Just (makeFailure ("missing success marker, output: " ++ unwords outputs))

    failureReport caseId err =
      CaseReport {caseReportId = caseId, caseReportFailure = Just (makeFailure err)}

    makeFailure message =
      CaseFailure
        { caseFailureCategory = classifyCaseFailure message,
          caseFailureMessage = message
        }
