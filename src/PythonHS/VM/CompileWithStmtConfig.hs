module PythonHS.VM.CompileWithStmtConfig (CompileWithStmtConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.CompileExprResult (CompileExprResult)

data CompileWithStmtConfig = CompileWithStmtConfig
  { compileWithStmtBaseIndex :: Int,
    compileWithStmtInFunction :: Bool,
    compileWithStmtMaybeLoop :: Maybe (Int, Int),
    compileWithStmtCmExpr :: Expr,
    compileWithStmtMaybeVarName :: Maybe String,
    compileWithStmtBody :: [Stmt],
    compileWithStmtWithPos :: Position,
    compileWithStmtCompileStatements :: Int -> Bool -> Maybe (Int, Int) -> [Stmt] -> Either String CompileExprResult
  }
