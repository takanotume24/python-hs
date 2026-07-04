module PythonHS.VM.CompileFunctionDefStmtConfig (CompileFunctionDefStmtConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.CompileExprResult (CompileExprResult)

data CompileFunctionDefStmtConfig = CompileFunctionDefStmtConfig
  { compileFunctionDefStmtCompileStatements :: Int -> Bool -> Maybe (Int, Int) -> [Stmt] -> Either String CompileExprResult,
    compileFunctionDefStmtCompileExpr :: Int -> Expr -> Either String CompileExprResult,
    compileFunctionDefStmtPosDef :: Position,
    compileFunctionDefStmtDefaults :: [(String, Expr)],
    compileFunctionDefStmtBody :: [Stmt]
  }
