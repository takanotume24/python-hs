module PythonHS.Parser.ParseCallArgumentConfig (ParseCallArgumentConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseResult (ParseResult)

data ParseCallArgumentConfig = ParseCallArgumentConfig
  { parseCallArgumentExpr :: [Token] -> Either ParseError (ParseResult Expr),
    parseCallArgumentTokenStream :: [Token]
  }
