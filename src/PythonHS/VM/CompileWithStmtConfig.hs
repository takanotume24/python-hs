module PythonHS.VM.CompileWithStmtConfig (CompileWithStmtConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.CompileExprResult (CompileExprResult)
import PythonHS.VM.LoopContext (LoopContext)

data CompileWithStmtConfig = CompileWithStmtConfig
  { compileWithStmtBaseIndex :: Int,
    compileWithStmtInFunction :: Bool,
    compileWithStmtMaybeLoop :: Maybe LoopContext,
    compileWithStmtCmExpr :: Expr,
    compileWithStmtMaybeVarName :: Maybe String,
    compileWithStmtBody :: [Stmt],
    compileWithStmtWithPos :: Position,
    compileWithStmtCompileStatements :: Int -> Bool -> Maybe LoopContext -> [Stmt] -> Either String CompileExprResult
  }
