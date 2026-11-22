module PythonHS.VM.CompileSubscriptAssignConfig (CompileSubscriptAssignConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.CompileExprResult (CompileExprResult)

data CompileSubscriptAssignConfig = CompileSubscriptAssignConfig
  { compileSubscriptAssignCompileExpr :: Int -> Expr -> Either String CompileExprResult,
    compileSubscriptAssignBaseIndex :: Int,
    compileSubscriptAssignTarget :: Expr,
    compileSubscriptAssignValue :: Expr,
    compileSubscriptAssignPos :: Position
  }
