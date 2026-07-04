module PythonHS.VM.CompileExprItemsAtConfig (CompileExprItemsAtConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.VM.CompileExprResult (CompileExprResult)

data CompileExprItemsAtConfig = CompileExprItemsAtConfig
  { compileExprItemsAtCompileExpr :: Int -> Expr -> Either String CompileExprResult,
    compileExprItemsAtBaseIndex :: Int,
    compileExprItemsAtExprs :: [Expr]
  }
