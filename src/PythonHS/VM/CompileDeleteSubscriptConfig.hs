module PythonHS.VM.CompileDeleteSubscriptConfig (CompileDeleteSubscriptConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.CompileExprResult (CompileExprResult)

data CompileDeleteSubscriptConfig = CompileDeleteSubscriptConfig
  { compileDeleteSubscriptCompileExpr :: Int -> Expr -> Either String CompileExprResult,
    compileDeleteSubscriptBaseIndex :: Int,
    compileDeleteSubscriptTarget :: Expr,
    compileDeleteSubscriptPos :: Position
  }
