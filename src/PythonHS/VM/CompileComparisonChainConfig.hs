module PythonHS.VM.CompileComparisonChainConfig (CompileComparisonChainConfig (..)) where

import PythonHS.AST.BinaryOperator (BinaryOperator)
import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.CompileExprResult (CompileExprResult)

data CompileComparisonChainConfig = CompileComparisonChainConfig
  { compileComparisonChainCompileExpr :: Int -> Expr -> Either String CompileExprResult,
    compileComparisonChainBaseIndex :: Int,
    compileComparisonChainLeft :: Expr,
    compileComparisonChainOps :: [BinaryOperator],
    compileComparisonChainRights :: [Expr],
    compileComparisonChainPos :: Position
  }
