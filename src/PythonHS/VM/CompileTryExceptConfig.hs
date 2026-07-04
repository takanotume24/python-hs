module PythonHS.VM.CompileTryExceptConfig (CompileTryExceptConfig (..)) where

import PythonHS.AST.Stmt (Stmt)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.CompileExprResult (CompileExprResult)

data CompileTryExceptConfig = CompileTryExceptConfig
  { compileTryExceptCompileStatements :: Int -> Bool -> Maybe (Int, Int) -> [Stmt] -> Either String CompileExprResult,
    compileTryExceptBaseIndex :: Int,
    compileTryExceptInFunction :: Bool,
    compileTryExceptMaybeLoop :: Maybe (Int, Int),
    compileTryExceptTryStmts :: [Stmt],
    compileTryExceptExceptClauses :: [(Maybe String, Maybe String, [Stmt], Position)],
    compileTryExceptMaybeFinally :: Maybe [Stmt]
  }
