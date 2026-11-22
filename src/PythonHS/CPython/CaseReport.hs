module PythonHS.CPython.CaseReport (CaseReport (..)) where

import PythonHS.CPython.CaseFailure (CaseFailure)
import PythonHS.CPython.CaseId (CaseId)

-- | Outcome of running one allowlisted CPython test method.
data CaseReport = CaseReport
  { caseReportId :: CaseId,
    caseReportFailure :: Maybe CaseFailure
  }
  deriving (Eq, Show)
