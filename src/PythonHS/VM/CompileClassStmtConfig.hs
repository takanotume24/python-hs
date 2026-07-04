module PythonHS.VM.CompileClassStmtConfig (CompileClassStmtConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.VM.CompileExprResult (CompileExprResult)
import PythonHS.VM.Instruction (Instruction)

data CompileClassStmtConfig = CompileClassStmtConfig
  { compileClassStmtCompileDefaults :: (Int -> Expr -> Either String CompileExprResult) -> [(String, Expr)] -> Either String ([(String, [Instruction])], Int),
    compileClassStmtCompileStatements :: Int -> Bool -> Maybe (Int, Int) -> [Stmt] -> Either String CompileExprResult,
    compileClassStmtCompileExpr :: Int -> Expr -> Either String CompileExprResult,
    compileClassStmtBaseIndex :: Int,
    compileClassStmtClassName :: String,
    compileClassStmtMaybeBase :: Maybe String,
    compileClassStmtBody :: [Stmt],
    compileClassStmtMaybeDataclass :: Maybe (Bool, Bool)
  }
