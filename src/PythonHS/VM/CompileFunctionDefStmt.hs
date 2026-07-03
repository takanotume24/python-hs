module PythonHS.VM.CompileFunctionDefStmt (compileFunctionDefStmt) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Evaluator.Value (Value (IntValue, ListValue))
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.CompileDefaults (compileDefaults)
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.HasYieldInStmts (hasYieldInStmts)
import PythonHS.VM.Instruction (Instruction (LoadName, PushConst, ReturnTop, StoreName), Instruction)

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
        pure ([PushConst (ListValue []), StoreName "__yield_acc__"] ++ compileExprResultCode bodyResult ++ [LoadName "__yield_acc__" posDef, ReturnTop])
      else do
        bodyResult <- compileStatements 0 True Nothing body
        pure (compileExprResultCode bodyResult ++ [PushConst (IntValue 0), ReturnTop])
  pure (functionCode, compiledDefaults)
