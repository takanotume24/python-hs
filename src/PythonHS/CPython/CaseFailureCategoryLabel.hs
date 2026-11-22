module PythonHS.CPython.CaseFailureCategoryLabel (caseFailureCategoryLabel) where

import PythonHS.CPython.CaseFailureCategory (CaseFailureCategory (..))

-- | Stable lowercase label used in text and JSON reports.
caseFailureCategoryLabel :: CaseFailureCategory -> String
caseFailureCategoryLabel category =
  case category of
    ParseFailure -> "parse"
    ImportFailure -> "import"
    AssertionFailure -> "assertion"
    RuntimeFailure -> "runtime"
    MissingMarkerFailure -> "marker"
    OtherFailure -> "other"
