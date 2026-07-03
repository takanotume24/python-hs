module PythonHS.Parser.ParseCallArgumentConfig (ParseCallArgumentConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)

data ParseCallArgumentConfig = ParseCallArgumentConfig
  { parseCallArgumentExpr :: [Token] -> Either ParseError (Expr, [Token])
  }
