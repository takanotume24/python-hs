module PythonHS.VM.CompileForStmtConfig (CompileForStmtConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.CompileExprResult (CompileExprResult)
import PythonHS.VM.LoopContext (LoopContext)

data CompileForStmtConfig = CompileForStmtConfig
  { compileForStmtCompileExpr :: Int -> Expr -> Either String CompileExprResult,
    compileForStmtCompileStatements :: Int -> Bool -> Maybe LoopContext -> [Stmt] -> Either String CompileExprResult,
    compileForStmtBaseIndex :: Int,
    compileForStmtInFunction :: Bool,
    compileForStmtName :: String,
    compileForStmtIter :: Expr,
    compileForStmtBody :: [Stmt],
    compileForStmtMaybeElse :: Maybe [Stmt],
    compileForStmtPos :: Position
  }
