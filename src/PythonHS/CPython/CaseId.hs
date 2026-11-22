module PythonHS.CPython.CaseId (CaseId (..)) where

-- | Identifier of a single CPython test method.
data CaseId = CaseId
  { caseIdFile :: FilePath,
    caseIdClassName :: String,
    caseIdMethodName :: String
  }
  deriving (Eq, Show)
