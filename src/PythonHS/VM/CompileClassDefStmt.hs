module PythonHS.VM.CompileClassDefStmt (compileClassDefStmt) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.VM.CompileClassStmt (compileClassStmt)
import PythonHS.VM.Instruction (Instruction)

compileClassDefStmt ::
  ((Int -> Expr -> Either String ([Instruction], Int)) -> [(String, Expr)] -> Either String ([(String, [Instruction])], Int)) ->
  (Int -> Bool -> Maybe (Int, Int) -> [Stmt] -> Either String ([Instruction], Int)) ->
  (Int -> Expr -> Either String ([Instruction], Int)) ->
  Int ->
  String ->
  Maybe String ->
  [Stmt] ->
  Maybe (Bool, Bool) ->
  Either String ([Instruction], Int)
compileClassDefStmt compileDefaults compileStatements compileExprAt baseIndex className maybeBase body maybeDataclass =
  compileClassStmt compileDefaults compileStatements compileExprAt baseIndex className maybeBase body maybeDataclass
