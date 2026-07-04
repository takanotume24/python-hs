module PythonHS.VM.CompileMatchConfig (CompileMatchConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Pattern (Pattern)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.CompileExprResult (CompileExprResult)

data CompileMatchConfig = CompileMatchConfig
  { compileMatchCompileExpr :: Int -> Expr -> Either String CompileExprResult,
    compileMatchCompileStatements :: Int -> Bool -> Maybe (Int, Int) -> [Stmt] -> Either String CompileExprResult,
    compileMatchBaseIndex :: Int,
    compileMatchInFunction :: Bool,
    compileMatchMaybeLoop :: Maybe (Int, Int),
    compileMatchSubjectExpr :: Expr,
    compileMatchCases :: [(Pattern, Maybe Expr, [Stmt], Position)]
  }
