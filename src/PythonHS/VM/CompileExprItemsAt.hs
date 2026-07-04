module PythonHS.VM.CompileExprItemsAt (compileExprItemsAt) where

import PythonHS.AST.Expr (Expr)
import PythonHS.VM.CompileExprItemsAtConfig (CompileExprItemsAtConfig (..))
import PythonHS.VM.CompileExprResult (CompileExprResult (..))

compileExprItemsAt :: CompileExprItemsAtConfig -> Either String CompileExprResult
compileExprItemsAt config =
  let compileExpr = compileExprItemsAtCompileExpr config
      baseIndex = compileExprItemsAtBaseIndex config
      exprs = compileExprItemsAtExprs config
   in case exprs of
        [] -> Right (CompileExprResult {compileExprResultCode = [], compileExprResultEndIndex = baseIndex})
        expr : rest -> do
          exprResult <- compileExpr baseIndex expr
          restResult <- compileExprItemsAt config {compileExprItemsAtCompileExpr = compileExpr, compileExprItemsAtBaseIndex = compileExprResultEndIndex exprResult, compileExprItemsAtExprs = rest}
          pure (CompileExprResult {compileExprResultCode = compileExprResultCode exprResult ++ compileExprResultCode restResult, compileExprResultEndIndex = compileExprResultEndIndex restResult})
