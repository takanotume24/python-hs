module PythonHS.VM.CompileExprItemsAt (compileExprItemsAt) where

import PythonHS.AST.Expr (Expr)
import PythonHS.VM.Instruction (Instruction)

compileExprItemsAt :: (Int -> Expr -> Either String ([Instruction], Int)) -> Int -> [Expr] -> Either String ([Instruction], Int)
compileExprItemsAt compileExprAt baseIndex exprs =
  case exprs of
    [] -> Right ([], baseIndex)
    expr : rest -> do
      (exprCode, exprEnd) <- compileExprAt baseIndex expr
      (restCode, restEnd) <- compileExprItemsAt compileExprAt exprEnd rest
      Right (exprCode ++ restCode, restEnd)
