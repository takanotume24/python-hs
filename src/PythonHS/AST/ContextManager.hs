module PythonHS.AST.ContextManager (ContextManager (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Position (Position)

-- | Record to encapsulate context manager data
data ContextManager = ContextManager
  { contextManagerExpr :: Expr,
    contextManagerVarName :: Maybe String,
    contextManagerPos :: Position
  }
  deriving (Eq, Show)
