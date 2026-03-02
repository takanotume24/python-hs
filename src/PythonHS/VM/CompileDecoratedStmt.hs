module PythonHS.VM.CompileDecoratedStmt (compileDecoratedStmt) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt (ClassDefStmt, FunctionDefDefaultsStmt, FunctionDefStmt), Stmt)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.VM.CompileDecoratorApplications (compileDecoratorApplications)
import PythonHS.VM.Instruction (Instruction)
import PythonHS.VM.StmtPosition (stmtPosition)

compileDecoratedStmt ::
  (Int -> Bool -> Maybe (Int, Int) -> Stmt -> Either String ([Instruction], Int)) ->
  (Int -> Expr -> Either String ([Instruction], Int)) ->
  Int ->
  Bool ->
  Maybe (Int, Int) ->
  [Expr] ->
  Stmt ->
  Either String ([Instruction], Int)
compileDecoratedStmt compileStmt compileExprAt baseIndex inFunction maybeLoop decorators targetStmt = do
  (targetCode, targetEnd) <- compileStmt baseIndex inFunction maybeLoop targetStmt
  targetName <- decoratedTargetName targetStmt
  (decoratorCode, decoratorEnd) <- compileDecoratorApplications compileExprAt targetEnd targetName decorators
  pure (targetCode ++ decoratorCode, decoratorEnd)
  where
    decoratedTargetName stmt =
      case stmt of
        FunctionDefStmt name _ _ _ -> Right name
        FunctionDefDefaultsStmt name _ _ _ _ -> Right name
        ClassDefStmt name _ _ _ -> Right name
        _ -> Left ("VM compile error: unsupported decorator target at " ++ showPos (stmtPosition stmt))
