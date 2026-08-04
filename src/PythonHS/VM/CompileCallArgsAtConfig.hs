module PythonHS.VM.CompileCallArgsAtConfig (CompileCallArgsAtConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.VM.CompileExprResult (CompileExprResult)

data CompileCallArgsAtConfig = CompileCallArgsAtConfig
  { compileCallArgsAtCompileExpr :: Int -> Expr -> Either String CompileExprResult,
    compileCallArgsAtArgs :: [Expr]
  }
