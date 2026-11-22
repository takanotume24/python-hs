module PythonHS.CPython.ReportLines (reportLines) where

import PythonHS.CPython.CaseFailure (CaseFailure (..))
import PythonHS.CPython.CaseFailureCategoryLabel (caseFailureCategoryLabel)
import PythonHS.CPython.CaseReport (CaseReport (..))
import PythonHS.CPython.FormatCaseId (formatCaseId)

-- | Render a human-readable report, one line per test case.
reportLines :: [CaseReport] -> [String]
reportLines = map renderReport
  where
    renderReport CaseReport {caseReportId = caseId, caseReportFailure = Nothing} =
      "PASS " ++ formatCaseId caseId
    renderReport CaseReport {caseReportId = caseId, caseReportFailure = Just failure} =
      "FAIL "
        ++ formatCaseId caseId
        ++ " ["
        ++ caseFailureCategoryLabel (caseFailureCategory failure)
        ++ "] "
        ++ caseFailureMessage failure
