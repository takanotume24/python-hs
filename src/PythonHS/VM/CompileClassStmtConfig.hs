module PythonHS.VM.CompileClassStmtConfig (CompileClassStmtConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.FunctionDefault (FunctionDefault)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.VM.CompileDefaultsResult (CompileDefaultsResult)
import PythonHS.VM.CompileExprResult (CompileExprResult)
import PythonHS.VM.DataclassFlags (DataclassFlags)
import PythonHS.VM.LoopContext (LoopContext)

data CompileClassStmtConfig = CompileClassStmtConfig
  { compileClassStmtCompileDefaults :: (Int -> Expr -> Either String CompileExprResult) -> [FunctionDefault Expr] -> Either String CompileDefaultsResult,
    compileClassStmtCompileStatements :: Int -> Bool -> Maybe LoopContext -> [Stmt] -> Either String CompileExprResult,
    compileClassStmtCompileExpr :: Int -> Expr -> Either String CompileExprResult,
    compileClassStmtBaseIndex :: Int,
    compileClassStmtClassName :: String,
    compileClassStmtMaybeBase :: Maybe String,
    compileClassStmtBody :: [Stmt],
    compileClassStmtMaybeDataclass :: Maybe DataclassFlags
  }
