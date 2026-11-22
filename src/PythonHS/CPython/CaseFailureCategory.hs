module PythonHS.CPython.CaseFailureCategory (CaseFailureCategory (..)) where

-- | Why a CPython test case could not be completed.
data CaseFailureCategory
  = ParseFailure
  | ImportFailure
  | AssertionFailure
  | RuntimeFailure
  | MissingMarkerFailure
  | OtherFailure
  deriving (Eq, Show)
