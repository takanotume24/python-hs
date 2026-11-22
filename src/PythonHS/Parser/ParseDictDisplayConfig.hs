module PythonHS.Parser.ParseDictDisplayConfig (ParseDictDisplayConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Position (Position)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseResult (ParseResult)

data ParseDictDisplayConfig = ParseDictDisplayConfig
  { parseDictDisplayExpr :: [Token] -> Either ParseError (ParseResult Expr),
    parseDictDisplayComprehension :: Expr -> [Token] -> Either ParseError (ParseResult Expr),
    parseDictDisplayPos :: Position,
    parseDictDisplayTokenStream :: [Token]
  }
