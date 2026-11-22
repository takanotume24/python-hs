module PythonHS.Parser.ParseFStringExprConfig (ParseFStringExprConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Position (Position)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseResult (ParseResult)

data ParseFStringExprConfig = ParseFStringExprConfig
  { parseFStringExprParseExpr :: [Token] -> Either ParseError (ParseResult Expr),
    parseFStringExprContent :: String,
    parseFStringExprPos :: Position
  }
