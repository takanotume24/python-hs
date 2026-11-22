module PythonHS.VM.CompileRaiseStmtConfig (CompileRaiseStmtConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.CompileExprResult (CompileExprResult)

data CompileRaiseStmtConfig = CompileRaiseStmtConfig
  { compileRaiseStmtCompileExpr :: Int -> Expr -> Either String CompileExprResult,
    compileRaiseStmtBaseIndex :: Int,
    compileRaiseStmtMaybeExpr :: Maybe Expr,
    compileRaiseStmtPos :: Position
  }
