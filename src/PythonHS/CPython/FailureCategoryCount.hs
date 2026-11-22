module PythonHS.CPython.FailureCategoryCount (FailureCategoryCount (..)) where

-- | Number of failing cases in one failure category.
data FailureCategoryCount = FailureCategoryCount
  { failureCategoryCountLabel :: String,
    failureCategoryCountValue :: Int
  }
  deriving (Eq, Show)
