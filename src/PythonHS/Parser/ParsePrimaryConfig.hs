module PythonHS.Parser.ParsePrimaryConfig (ParsePrimaryConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseResult (ParseResult)

data ParsePrimaryConfig = ParsePrimaryConfig
  { parsePrimaryExpr :: [Token] -> Either ParseError (ParseResult Expr),
    parsePrimaryTokenStream :: [Token]
  }
