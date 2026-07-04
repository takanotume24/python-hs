module PythonHS.VM.CompileExprAtConfig (CompileExprAtConfig (..)) where

import PythonHS.AST.Expr (Expr)

data CompileExprAtConfig = CompileExprAtConfig
  { compileExprAtBaseIndex :: Int,
    compileExprAtExpr :: Expr
  }
