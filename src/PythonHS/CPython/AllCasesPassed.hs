module PythonHS.CPython.AllCasesPassed (allCasesPassed) where

import PythonHS.CPython.CaseReport (CaseReport (..))

-- | Whether every reported test case passed.
allCasesPassed :: [CaseReport] -> Bool
allCasesPassed = all (\CaseReport {caseReportFailure = failure} -> failure == Nothing)
