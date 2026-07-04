module PythonHS.VM.CompileCompoundAssignConfig (CompileCompoundAssignConfig (..)) where

import PythonHS.AST.BinaryOperator (BinaryOperator)
import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.CompileExprResult (CompileExprResult)

data CompileCompoundAssignConfig = CompileCompoundAssignConfig
  { compileCompoundAssignCompileExpr :: Int -> Expr -> Either String CompileExprResult,
    compileCompoundAssignBaseIndex :: Int,
    compileCompoundAssignName :: String,
    compileCompoundAssignExpr :: Expr,
    compileCompoundAssignPos :: Position,
    compileCompoundAssignOp :: BinaryOperator
  }
