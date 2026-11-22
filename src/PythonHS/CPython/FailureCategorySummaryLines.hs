module PythonHS.CPython.FailureCategorySummaryLines (failureCategorySummaryLines) where

import Data.List (intercalate)
import PythonHS.CPython.CaseReport (CaseReport)
import PythonHS.CPython.CountFailureCategories (countFailureCategories)
import PythonHS.CPython.FailureCategoryCount (FailureCategoryCount (..))

-- | Render a single summary line of failure counts by category.
failureCategorySummaryLines :: [CaseReport] -> [String]
failureCategorySummaryLines reports =
  [ "failures by category: " ++ intercalate ", " entries
  | not (null entries)
  ]
  where
    entries =
      [ failureCategoryCountLabel count ++ "=" ++ show (failureCategoryCountValue count)
      | count <- countFailureCategories reports
      ]
