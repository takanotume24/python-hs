module PythonHS.VM.CompiledComprehensionClause (CompiledComprehensionClause (..)) where

data CompiledComprehensionClause instruction = CompiledComprehensionClause
  { compiledComprehensionClauseTargets :: [String],
    compiledComprehensionClauseIterCode :: [instruction],
    compiledComprehensionClauseConds :: [[instruction]]
  }
  deriving (Eq, Show)
