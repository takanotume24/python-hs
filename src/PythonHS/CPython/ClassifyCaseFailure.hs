module PythonHS.CPython.ClassifyCaseFailure (classifyCaseFailure) where

import Data.List (isInfixOf, isPrefixOf)
import PythonHS.CPython.CaseFailureCategory (CaseFailureCategory (..))

-- | Classify an error message produced by the CPython test runner.
classifyCaseFailure :: String -> CaseFailureCategory
classifyCaseFailure message
  | "Import error:" `isPrefixOf` message = ImportFailure
  | "AssertionError:" `isPrefixOf` message = AssertionFailure
  | "missing success marker" `isInfixOf` message = MissingMarkerFailure
  | any (`isPrefixOf` message) parseErrorPrefixes = ParseFailure
  | otherwise = RuntimeFailure
  where
    parseErrorPrefixes =
      [ "ExpectedExpression",
        "ExpectedAssignAfterIdentifier",
        "ExpectedNewlineAfterStatement",
        "UnexpectedCharacter"
      ]
