module PythonHS.VM.CompileExprItemsAt (compileExprItemsAt) where

import PythonHS.AST.Expr (Expr)
import PythonHS.VM.CompileExprResult (CompileExprResult (..))

compileExprItemsAt :: (Int -> Expr -> Either String CompileExprResult) -> Int -> [Expr] -> Either String CompileExprResult
compileExprItemsAt compileExprAt baseIndex exprs =
  case exprs of
    [] -> Right (CompileExprResult {compileExprResultCode = [], compileExprResultEndIndex = baseIndex})
    expr : rest -> do
      exprResult <- compileExprAt baseIndex expr
      restResult <- compileExprItemsAt compileExprAt (compileExprResultEndIndex exprResult) rest
      pure (CompileExprResult {compileExprResultCode = compileExprResultCode exprResult ++ compileExprResultCode restResult, compileExprResultEndIndex = compileExprResultEndIndex restResult})
