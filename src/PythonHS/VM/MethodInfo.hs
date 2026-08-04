module PythonHS.VM.MethodInfo (MethodInfo (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.FunctionDefault (FunctionDefault)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Lexer.Position (Position)

data MethodInfo = MethodInfo
  { methodInfoName :: String,
    methodInfoParams :: [String],
    methodInfoDefaults :: [FunctionDefault Expr],
    methodInfoBody :: [Stmt],
    methodInfoPos :: Position
  }
  deriving (Eq, Show)
