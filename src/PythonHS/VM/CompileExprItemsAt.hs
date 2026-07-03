module PythonHS.VM.CompileExprItemsAt (compileExprItemsAt) where

import PythonHS.AST.Expr (Expr)
import PythonHS.VM.CompileExprResult (CompileExprResult (..))
import PythonHS.VM.Instruction (Instruction)

compileExprItemsAt :: (Int -> Expr -> Either String CompileExprResult) -> Int -> [Expr] -> Either String CompileExprResult
compileExprItemsAt compileExprAt baseIndex exprs =
  case exprs of
    [] -> Right (CompileExprResult [] baseIndex)
    expr : rest -> do
      exprResult <- compileExprAt baseIndex expr
      restResult <- compileExprItemsAt compileExprAt (compileExprResultEndIndex exprResult) rest
      pure (CompileExprResult (compileExprResultCode exprResult ++ compileExprResultCode restResult) (compileExprResultEndIndex restResult))
