module PythonHS.VM.CompileFunctionDefStmt (compileFunctionDefStmt) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.CompileDefaults (compileDefaults)
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.HasYieldInStmts (hasYieldInStmts)
import PythonHS.VM.Instruction (Instruction (..))

compileFunctionDefStmt ::
  (Int -> Bool -> Maybe (Int, Int) -> [Stmt] -> Either String CompileExprResult) ->
  (Int -> Expr -> Either String CompileExprResult) ->
  Position ->
  [(String, Expr)] ->
  [Stmt] ->
  Either String ([Instruction], [(String, [Instruction])])
compileFunctionDefStmt compileStatements compileExprAt posDef defaults body = do
  (compiledDefaults, _) <- compileDefaults compileExprAt defaults
  functionCode <-
    if hasYieldInStmts body
      then do
        bodyResult <- compileStatements 2 True Nothing body
        pure ([PushConst {pushConstValue = ListValue {listValueItems = []}}, StoreName {storeNameName = "__yield_acc__"}] ++ compileExprResultCode bodyResult ++ [LoadName {loadNameName = "__yield_acc__", loadNamePos = posDef}, ReturnTop])
      else do
        bodyResult <- compileStatements 0 True Nothing body
        pure (compileExprResultCode bodyResult ++ [PushConst {pushConstValue = IntValue {intValue = 0}}, ReturnTop])
  pure (functionCode, compiledDefaults)
