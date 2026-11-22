module PythonHS.CPython.BuildFileDriverSource (buildFileDriverSource) where

import PythonHS.CPython.CaseId (CaseId (..))
import PythonHS.CPython.StripMainGuard (stripMainGuard)

-- | Build a driver that loads the whole CPython test file first.
--
-- Unlike 'buildDriverSource', the target method may use helpers that are
-- defined at module level or on the test class. The @__main__@ guard is
-- removed and a driver that runs only the requested method is appended.
buildFileDriverSource :: String -> CaseId -> Either String String
buildFileDriverSource source caseId =
  Right
    ( unlines
        ( lines (stripMainGuard source)
            ++ driverLines
        )
    )
  where
    driverLines =
      [ "__python_hs_case = " ++ caseIdClassName caseId ++ "()",
        "__python_hs_case.setUp()",
        "__python_hs_case." ++ caseIdMethodName caseId ++ "()",
        "__python_hs_case.tearDown()",
        "__python_hs_case.__python_hs_run_cleanups__()",
        "print(\"__PYTHON_HS_CPYTHON_TEST_OK__\")"
      ]
