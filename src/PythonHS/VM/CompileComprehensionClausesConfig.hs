module PythonHS.VM.CompileComprehensionClausesConfig (CompileComprehensionClausesConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.ListComprehensionClause (ListComprehensionClause)
import PythonHS.VM.CompileExprResult (CompileExprResult)

data CompileComprehensionClausesConfig = CompileComprehensionClausesConfig
  { compileComprehensionClausesCompileExpr :: Int -> Expr -> Either String CompileExprResult,
    compileComprehensionClausesClauses :: [ListComprehensionClause Expr]
  }
