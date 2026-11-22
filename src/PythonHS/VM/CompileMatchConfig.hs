module PythonHS.VM.CompileMatchConfig (CompileMatchConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.MatchCase (MatchCase)
import PythonHS.AST.Pattern (Pattern)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.VM.CompileExprResult (CompileExprResult)
import PythonHS.VM.LoopContext (LoopContext)

data CompileMatchConfig = CompileMatchConfig
  { compileMatchCompileExpr :: Int -> Expr -> Either String CompileExprResult,
    compileMatchCompileStatements :: Int -> Bool -> Maybe LoopContext -> [Stmt] -> Either String CompileExprResult,
    compileMatchBaseIndex :: Int,
    compileMatchInFunction :: Bool,
    compileMatchMaybeLoop :: Maybe LoopContext,
    compileMatchSubjectExpr :: Expr,
    compileMatchCases :: [MatchCase Expr Pattern Stmt]
  }
