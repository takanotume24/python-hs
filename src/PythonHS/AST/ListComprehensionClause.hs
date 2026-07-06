module PythonHS.AST.ListComprehensionClause (ListComprehensionClause (..)) where

data ListComprehensionClause e = ListComprehensionClause
  { listComprehensionClauseTargets :: [String],
    listComprehensionClauseIter :: e,
    listComprehensionClauseConds :: [e]
  }
  deriving (Eq, Show)
