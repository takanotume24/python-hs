module PythonHS.VM.CompileFunctionDefStmtConfig (CompileFunctionDefStmtConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.FunctionDefault (FunctionDefault)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.CompileExprResult (CompileExprResult)
import PythonHS.VM.LoopContext (LoopContext)

data CompileFunctionDefStmtConfig = CompileFunctionDefStmtConfig
  { compileFunctionDefStmtCompileStatements :: Int -> Bool -> Maybe LoopContext -> [Stmt] -> Either String CompileExprResult,
    compileFunctionDefStmtCompileExpr :: Int -> Expr -> Either String CompileExprResult,
    compileFunctionDefStmtPosDef :: Position,
    compileFunctionDefStmtDefaults :: [FunctionDefault Expr],
    compileFunctionDefStmtBody :: [Stmt]
  }
