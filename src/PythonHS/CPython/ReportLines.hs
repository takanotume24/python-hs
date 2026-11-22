module PythonHS.CPython.ReportLines (reportLines) where

import PythonHS.CPython.CaseId (CaseId (..))
import PythonHS.CPython.CaseReport (CaseReport (..))

-- | Render a human-readable report, one line per test case.
reportLines :: [CaseReport] -> [String]
reportLines = map renderReport
  where
    renderReport CaseReport {caseReportId = caseId, caseReportFailure = Nothing} =
      "PASS " ++ renderCaseId caseId
    renderReport CaseReport {caseReportId = caseId, caseReportFailure = Just failure} =
      "FAIL " ++ renderCaseId caseId ++ ": " ++ failure

    renderCaseId CaseId {caseIdFile = file, caseIdClassName = className, caseIdMethodName = methodName} =
      file ++ "::" ++ className ++ "::" ++ methodName
