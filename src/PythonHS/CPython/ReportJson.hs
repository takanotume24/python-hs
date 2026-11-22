module PythonHS.CPython.ReportJson (reportJson) where

import Data.List (intercalate)
import PythonHS.CPython.CaseFailure (CaseFailure (..))
import PythonHS.CPython.CaseFailureCategoryLabel (caseFailureCategoryLabel)
import PythonHS.CPython.CaseId (CaseId (..))
import PythonHS.CPython.CaseReport (CaseReport (..))

-- | Render a machine-readable JSON report.
reportJson :: [CaseReport] -> String
reportJson reports =
  unlines
    [ "{",
      "  \"total\": " ++ show (length reports) ++ ",",
      "  \"passed\": " ++ show passedCount ++ ",",
      "  \"failed\": " ++ show failedCount ++ ",",
      "  \"cases\": [",
      intercalate ",\n" (map renderCase reports),
      "  ]",
      "}"
    ]
  where
    passedCount = length (filter isPassed reports)
    failedCount = length reports - passedCount

    isPassed CaseReport {caseReportFailure = Nothing} = True
    isPassed _ = False

    renderCase CaseReport {caseReportId = caseId, caseReportFailure = Nothing} =
      "    " ++ renderCaseObject caseId "pass" "null" "null"
    renderCase CaseReport {caseReportId = caseId, caseReportFailure = Just failure} =
      "    "
        ++ renderCaseObject
          caseId
          "fail"
          (jsonString (caseFailureCategoryLabel (caseFailureCategory failure)))
          (jsonString (caseFailureMessage failure))

    renderCaseObject CaseId {caseIdFile = file, caseIdClassName = className, caseIdMethodName = methodName} status category message =
      "{"
        ++ "\"file\":"
        ++ jsonString file
        ++ ",\"class\":"
        ++ jsonString className
        ++ ",\"method\":"
        ++ jsonString methodName
        ++ ",\"status\":"
        ++ jsonString status
        ++ ",\"category\":"
        ++ category
        ++ ",\"message\":"
        ++ message
        ++ "}"

    jsonString text = "\"" ++ concatMap escape text ++ "\""

    escape character =
      case character of
        '"' -> "\\\""
        '\\' -> "\\\\"
        '\n' -> "\\n"
        '\r' -> "\\r"
        '\t' -> "\\t"
        _ -> [character]
