module PythonHS.VM.CompileTryExceptConfig (CompileTryExceptConfig (..)) where

import PythonHS.AST.ExceptSuite (ExceptSuite)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.CompileExprResult (CompileExprResult)
import PythonHS.VM.LoopContext (LoopContext)

data CompileTryExceptConfig = CompileTryExceptConfig
  { compileTryExceptCompileStatements :: Int -> Bool -> Maybe LoopContext -> [Stmt] -> Either String CompileExprResult,
    compileTryExceptBaseIndex :: Int,
    compileTryExceptInFunction :: Bool,
    compileTryExceptMaybeLoop :: Maybe LoopContext,
    compileTryExceptTryStmts :: [Stmt],
    compileTryExceptExceptClauses :: [ExceptSuite Stmt],
    compileTryExceptMaybeFinally :: Maybe [Stmt]
  }
