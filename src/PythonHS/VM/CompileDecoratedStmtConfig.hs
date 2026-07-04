module PythonHS.VM.CompileDecoratedStmtConfig (CompileDecoratedStmtConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.VM.CompileExprResult (CompileExprResult)

data CompileDecoratedStmtConfig = CompileDecoratedStmtConfig
  { compileDecoratedStmtCompileStmt :: Int -> Bool -> Maybe (Int, Int) -> Stmt -> Either String CompileExprResult,
    compileDecoratedStmtCompileDataclassClass :: Int -> String -> Maybe String -> [Stmt] -> Maybe (Bool, Bool) -> Either String CompileExprResult,
    compileDecoratedStmtCompileExpr :: Int -> Expr -> Either String CompileExprResult,
    compileDecoratedStmtBaseIndex :: Int,
    compileDecoratedStmtInFunction :: Bool,
    compileDecoratedStmtMaybeLoop :: Maybe (Int, Int),
    compileDecoratedStmtDecorators :: [Expr],
    compileDecoratedStmtTargetStmt :: Stmt
  }
