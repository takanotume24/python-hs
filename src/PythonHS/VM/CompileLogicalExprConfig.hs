module PythonHS.VM.CompileLogicalExprConfig (CompileLogicalExprConfig (..)) where

import PythonHS.AST.BinaryOperator (BinaryOperator)
import PythonHS.AST.Expr (Expr)
import PythonHS.VM.CompileExprResult (CompileExprResult)

data CompileLogicalExprConfig = CompileLogicalExprConfig
  { compileLogicalExprCompileExpr :: Int -> Expr -> Either String CompileExprResult,
    compileLogicalExprOp :: BinaryOperator,
    compileLogicalExprBaseIndex :: Int,
    compileLogicalExprLeft :: Expr,
    compileLogicalExprRight :: Expr
  }
