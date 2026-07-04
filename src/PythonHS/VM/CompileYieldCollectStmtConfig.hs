module PythonHS.VM.CompileYieldCollectStmtConfig (CompileYieldCollectStmtConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.CompileExprResult (CompileExprResult)

data CompileYieldCollectStmtConfig = CompileYieldCollectStmtConfig
  { compileYieldCollectStmtCompileExpr :: Int -> Expr -> Either String CompileExprResult,
    compileYieldCollectStmtBaseIndex :: Int,
    compileYieldCollectStmtOperation :: String,
    compileYieldCollectStmtYieldedExpr :: Expr,
    compileYieldCollectStmtPos :: Position
  }
