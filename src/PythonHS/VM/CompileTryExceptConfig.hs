module PythonHS.VM.CompileTryExceptConfig (CompileTryExceptConfig (..)) where

import PythonHS.AST.ExceptSuite (ExceptSuite)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.VM.CompileExprResult (CompileExprResult)
import PythonHS.VM.LoopContext (LoopContext)

data CompileTryExceptConfig = CompileTryExceptConfig
  { compileTryExceptCompileStatements :: Int -> Bool -> Maybe LoopContext -> [Stmt] -> Either String CompileExprResult,
    compileTryExceptBaseIndex :: Int,
    compileTryExceptInFunction :: Bool,
    compileTryExceptMaybeLoop :: Maybe LoopContext,
    compileTryExceptTryStmts :: [Stmt],
    compileTryExceptExceptClauses :: [ExceptSuite Stmt],
    compileTryExceptMaybeElse :: Maybe [Stmt],
    compileTryExceptMaybeFinally :: Maybe [Stmt]
  }
