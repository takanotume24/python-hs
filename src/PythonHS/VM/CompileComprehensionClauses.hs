module PythonHS.VM.CompileComprehensionClauses (compileComprehensionClauses) where

import PythonHS.AST.Expr (Expr)
import PythonHS.VM.Instruction (Instruction (ReturnTop))

compileComprehensionClauses ::
  (Int -> Expr -> Either String ([Instruction], Int)) ->
  [([String], Expr, [Expr])] ->
  Either String [([String], [Instruction], [[Instruction]])]
compileComprehensionClauses compileExprAt clauses =
  case clauses of
    [] -> Right []
    (targets, iterExpr, condExprs) : rest -> do
      (iterCode, _) <- compileExprAt 0 iterExpr
      condCodes <- compileConditions condExprs
      restClauses <- compileComprehensionClauses compileExprAt rest
      Right ((targets, iterCode ++ [ReturnTop], condCodes) : restClauses)
  where
    compileConditions [] = Right []
    compileConditions (condExpr : restConds) = do
      (condCode, _) <- compileExprAt 0 condExpr
      restCodes <- compileConditions restConds
      Right ((condCode ++ [ReturnTop]) : restCodes)
