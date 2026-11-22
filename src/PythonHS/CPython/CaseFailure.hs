module PythonHS.CPython.CaseFailure (CaseFailure (..)) where

import PythonHS.CPython.CaseFailureCategory (CaseFailureCategory)

-- | A classified failure for one CPython test case.
data CaseFailure = CaseFailure
  { caseFailureCategory :: CaseFailureCategory,
    caseFailureMessage :: String
  }
  deriving (Eq, Show)
