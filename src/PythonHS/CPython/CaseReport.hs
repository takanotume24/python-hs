module PythonHS.CPython.CaseReport (CaseReport (..)) where

import PythonHS.CPython.CaseId (CaseId)

-- | Outcome of running one allowlisted CPython test method.
data CaseReport = CaseReport
  { caseReportId :: CaseId,
    caseReportFailure :: Maybe String
  }
  deriving (Eq, Show)
