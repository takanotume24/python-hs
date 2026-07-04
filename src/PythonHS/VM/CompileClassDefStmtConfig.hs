module PythonHS.VM.CompileClassDefStmtConfig (CompileClassDefStmtConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.VM.CompileExprResult (CompileExprResult)
import PythonHS.VM.Instruction (Instruction)

data CompileClassDefStmtConfig = CompileClassDefStmtConfig
  { compileClassDefStmtCompileDefaults :: (Int -> Expr -> Either String CompileExprResult) -> [(String, Expr)] -> Either String ([(String, [Instruction])], Int),
    compileClassDefStmtCompileStatements :: Int -> Bool -> Maybe (Int, Int) -> [Stmt] -> Either String CompileExprResult,
    compileClassDefStmtCompileExpr :: Int -> Expr -> Either String CompileExprResult,
    compileClassDefStmtBaseIndex :: Int,
    compileClassDefStmtClassName :: String,
    compileClassDefStmtMaybeBase :: Maybe String,
    compileClassDefStmtBody :: [Stmt],
    compileClassDefStmtMaybeDataclass :: Maybe (Bool, Bool)
  }
