module PythonHS.VM.CompileDecoratedStmtConfig (CompileDecoratedStmtConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.VM.CompileExprResult (CompileExprResult)
import PythonHS.VM.DataclassFlags (DataclassFlags)
import PythonHS.VM.LoopContext (LoopContext)

data CompileDecoratedStmtConfig = CompileDecoratedStmtConfig
  { compileDecoratedStmtCompileStmt :: Int -> Bool -> Maybe LoopContext -> Stmt -> Either String CompileExprResult,
    compileDecoratedStmtCompileDataclassClass :: Int -> String -> Maybe String -> [Stmt] -> Maybe DataclassFlags -> Either String CompileExprResult,
    compileDecoratedStmtCompileExpr :: Int -> Expr -> Either String CompileExprResult,
    compileDecoratedStmtBaseIndex :: Int,
    compileDecoratedStmtInFunction :: Bool,
    compileDecoratedStmtMaybeLoop :: Maybe LoopContext,
    compileDecoratedStmtDecorators :: [Expr],
    compileDecoratedStmtTargetStmt :: Stmt
  }
