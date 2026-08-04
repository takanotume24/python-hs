module PythonHS.VM.CompileClassDefStmtConfig (CompileClassDefStmtConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.FunctionDefault (FunctionDefault)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.VM.CompileDefaultsResult (CompileDefaultsResult)
import PythonHS.VM.CompileExprResult (CompileExprResult)
import PythonHS.VM.DataclassFlags (DataclassFlags)
import PythonHS.VM.LoopContext (LoopContext)

data CompileClassDefStmtConfig = CompileClassDefStmtConfig
  { compileClassDefStmtCompileDefaults :: (Int -> Expr -> Either String CompileExprResult) -> [FunctionDefault Expr] -> Either String CompileDefaultsResult,
    compileClassDefStmtCompileStatements :: Int -> Bool -> Maybe LoopContext -> [Stmt] -> Either String CompileExprResult,
    compileClassDefStmtCompileExpr :: Int -> Expr -> Either String CompileExprResult,
    compileClassDefStmtBaseIndex :: Int,
    compileClassDefStmtClassName :: String,
    compileClassDefStmtMaybeBase :: Maybe String,
    compileClassDefStmtBody :: [Stmt],
    compileClassDefStmtMaybeDataclass :: Maybe DataclassFlags
  }
