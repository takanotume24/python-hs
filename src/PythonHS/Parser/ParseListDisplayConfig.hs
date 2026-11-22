module PythonHS.Parser.ParseListDisplayConfig (ParseListDisplayConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Position (Position)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseResult (ParseResult)

data ParseListDisplayConfig = ParseListDisplayConfig
  { parseListDisplayExpr :: [Token] -> Either ParseError (ParseResult Expr),
    parseListDisplayPos :: Position,
    parseListDisplayTokenStream :: [Token]
  }
