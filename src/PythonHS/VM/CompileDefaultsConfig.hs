module PythonHS.VM.CompileDefaultsConfig (CompileDefaultsConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.FunctionDefault (FunctionDefault)
import PythonHS.VM.CompileExprResult (CompileExprResult)
import PythonHS.VM.Instruction (Instruction)

data CompileDefaultsConfig = CompileDefaultsConfig
  { compileDefaultsCompileExpr :: Int -> Expr -> Either String CompileExprResult,
    compileDefaultsDefaults :: [FunctionDefault Expr]
  }
