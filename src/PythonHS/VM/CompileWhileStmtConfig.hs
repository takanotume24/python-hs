module PythonHS.VM.CompileWhileStmtConfig (CompileWhileStmtConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.CompileExprResult (CompileExprResult)
import PythonHS.VM.LoopContext (LoopContext)

data CompileWhileStmtConfig = CompileWhileStmtConfig
  { compileWhileStmtCompileExpr :: Int -> Expr -> Either String CompileExprResult,
    compileWhileStmtCompileStatements :: Int -> Bool -> Maybe LoopContext -> [Stmt] -> Either String CompileExprResult,
    compileWhileStmtBaseIndex :: Int,
    compileWhileStmtInFunction :: Bool,
    compileWhileStmtCond :: Expr,
    compileWhileStmtBody :: [Stmt],
    compileWhileStmtMaybeElse :: Maybe [Stmt],
    compileWhileStmtPos :: Position
  }
