module PythonHS.VM.CompileTryExceptConfig (CompileTryExceptConfig (..)) where

import PythonHS.AST.ExceptSuite (ExceptSuite)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.CompileExprResult (CompileExprResult)

data CompileTryExceptConfig = CompileTryExceptConfig
  { compileTryExceptCompileStatements :: Int -> Bool -> Maybe (Int, Int) -> [Stmt] -> Either String CompileExprResult,
    compileTryExceptBaseIndex :: Int,
    compileTryExceptInFunction :: Bool,
    compileTryExceptMaybeLoop :: Maybe (Int, Int),
    compileTryExceptTryStmts :: [Stmt],
    compileTryExceptExceptClauses :: [ExceptSuite Stmt],
    compileTryExceptMaybeFinally :: Maybe [Stmt]
  }
