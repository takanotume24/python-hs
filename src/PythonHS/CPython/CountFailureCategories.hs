module PythonHS.CPython.CountFailureCategories (countFailureCategories) where

import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import PythonHS.CPython.CaseFailure (CaseFailure (..))
import PythonHS.CPython.CaseFailureCategoryLabel (caseFailureCategoryLabel)
import PythonHS.CPython.CaseReport (CaseReport (..))
import PythonHS.CPython.FailureCategoryCount (FailureCategoryCount (..))

-- | Count failures per category, ordered by label so reports stay stable.
countFailureCategories :: [CaseReport] -> [FailureCategoryCount]
countFailureCategories reports =
  sortOn
    failureCategoryCountLabel
    [ FailureCategoryCount
        { failureCategoryCountLabel = label,
          failureCategoryCountValue = count
        }
    | (label, count) <- Map.toList (foldl addFailure Map.empty reports)
    ]
  where
    addFailure counts CaseReport {caseReportFailure = Nothing} = counts
    addFailure counts CaseReport {caseReportFailure = Just failure} =
      Map.insertWith (+) (caseFailureCategoryLabel (caseFailureCategory failure)) 1 counts
