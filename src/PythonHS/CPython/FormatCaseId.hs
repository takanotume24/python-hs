module PythonHS.CPython.FormatCaseId (formatCaseId) where

import PythonHS.CPython.CaseId (CaseId (..))

-- | Render a case identifier in allowlist form.
formatCaseId :: CaseId -> String
formatCaseId CaseId {caseIdFile = file, caseIdClassName = className, caseIdMethodName = methodName} =
  file ++ "::" ++ className ++ "::" ++ methodName
