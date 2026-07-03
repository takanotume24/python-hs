module PythonHS.VM.CompileComprehensionClauses (compileComprehensionClauses) where

import PythonHS.AST.Expr (Expr)
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.Instruction (Instruction (ReturnTop))

compileComprehensionClauses ::
  (Int -> Expr -> Either String CompileExprResult) ->
  [([String], Expr, [Expr])] ->
  Either String [([String], [Instruction], [[Instruction]])]
compileComprehensionClauses compileExprAt clauses =
  case clauses of
    [] -> Right []
    (targets, iterExpr, condExprs) : rest -> do
      iterResult <- compileExprAt 0 iterExpr
      condCodes <- compileConditions condExprs
      restClauses <- compileComprehensionClauses compileExprAt rest
      Right ((targets, compileExprResultCode iterResult ++ [ReturnTop], condCodes) : restClauses)
  where
    compileConditions [] = Right []
    compileConditions (condExpr : restConds) = do
      condResult <- compileExprAt 0 condExpr
      restCodes <- compileConditions restConds
      Right ((compileExprResultCode condResult ++ [ReturnTop]) : restCodes)
