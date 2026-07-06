module PythonHS.Parser.ParseCallArgumentResult (ParseCallArgumentResult (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Position (Position)

data ParseCallArgumentResult = ParseCallArgumentResult
  { parseCallArgumentResultExpr :: Expr,
    parseCallArgumentResultIsKeyword :: Bool,
    parseCallArgumentResultPos :: Position
  }
  deriving (Eq, Show)
