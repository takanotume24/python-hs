module PythonHS.VM.CompileComprehensionClauses (compileComprehensionClauses) where

import PythonHS.AST.Expr (Expr)
import PythonHS.VM.CompileComprehensionClausesConfig (CompileComprehensionClausesConfig (..))
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.Instruction (Instruction (ReturnTop))

compileComprehensionClauses :: CompileComprehensionClausesConfig -> Either String [([String], [Instruction], [[Instruction]])]
compileComprehensionClauses config = case clauses of
  [] -> Right []
  (targets, iterExpr, condExprs) : rest -> do
    iterResult <- compileExpr 0 iterExpr
    condCodes <- compileConditions condExprs
    restClauses <- compileComprehensionClauses config {compileComprehensionClausesClauses = rest}
    Right ((targets, compileExprResultCode iterResult ++ [ReturnTop], condCodes) : restClauses)
  where
    compileExpr = compileComprehensionClausesCompileExpr config
    clauses = compileComprehensionClausesClauses config
    compileConditions [] = Right []
    compileConditions (condExpr : restConds) = do
      condResult <- compileExpr 0 condExpr
      restCodes <- compileConditions restConds
      Right ((compileExprResultCode condResult ++ [ReturnTop]) : restCodes)
