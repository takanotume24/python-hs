module PythonHS.VM.CompileClassDefStmt (compileClassDefStmt) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.VM.CompileClassStmt (compileClassStmt)
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.Instruction (Instruction)

compileClassDefStmt ::
  ((Int -> Expr -> Either String CompileExprResult) -> [(String, Expr)] -> Either String ([(String, [Instruction])], Int)) ->
  (Int -> Bool -> Maybe (Int, Int) -> [Stmt] -> Either String CompileExprResult) ->
  (Int -> Expr -> Either String CompileExprResult) ->
  Int ->
  String ->
  Maybe String ->
  [Stmt] ->
  Maybe (Bool, Bool) ->
  Either String CompileExprResult
compileClassDefStmt compileDefaults compileStatements compileExprAt baseIndex className maybeBase body maybeDataclass =
  compileClassStmt compileDefaults compileStatements compileExprAt baseIndex className maybeBase body maybeDataclass
