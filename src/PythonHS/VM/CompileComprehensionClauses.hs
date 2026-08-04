module PythonHS.VM.CompileComprehensionClauses (compileComprehensionClauses) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.ListComprehensionClause (ListComprehensionClause (..))
import PythonHS.VM.CompileComprehensionClausesConfig (CompileComprehensionClausesConfig (..))
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.CompiledComprehensionClause (CompiledComprehensionClause (..))
import PythonHS.VM.Instruction (Instruction (ReturnTop))

compileComprehensionClauses :: CompileComprehensionClausesConfig -> Either String [CompiledComprehensionClause Instruction]
compileComprehensionClauses config = case clauses of
  [] -> Right []
  ListComprehensionClause {listComprehensionClauseTargets = targets, listComprehensionClauseIter = iterExpr, listComprehensionClauseConds = condExprs} : rest -> do
    iterResult <- compileExpr 0 iterExpr
    condCodes <- compileConditions condExprs
    restClauses <- compileComprehensionClauses config {compileComprehensionClausesClauses = rest}
    Right (CompiledComprehensionClause {compiledComprehensionClauseTargets = targets, compiledComprehensionClauseIterCode = compileExprResultCode iterResult ++ [ReturnTop], compiledComprehensionClauseConds = condCodes} : restClauses)
  where
    compileExpr = compileComprehensionClausesCompileExpr config
    clauses = compileComprehensionClausesClauses config
    compileConditions [] = Right []
    compileConditions (condExpr : restConds) = do
      condResult <- compileExpr 0 condExpr
      restCodes <- compileConditions restConds
      Right ((compileExprResultCode condResult ++ [ReturnTop]) : restCodes)
