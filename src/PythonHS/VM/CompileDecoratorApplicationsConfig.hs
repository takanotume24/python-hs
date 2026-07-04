module PythonHS.VM.CompileDecoratorApplicationsConfig (CompileDecoratorApplicationsConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.VM.CompileExprResult (CompileExprResult)

data CompileDecoratorApplicationsConfig = CompileDecoratorApplicationsConfig
  { compileDecoratorApplicationsCompileExpr :: Int -> Expr -> Either String CompileExprResult,
    compileDecoratorApplicationsBaseIndex :: Int,
    compileDecoratorApplicationsTargetName :: String,
    compileDecoratorApplicationsDecorators :: [Expr]
  }
