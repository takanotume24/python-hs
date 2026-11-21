module PythonHS.Parser.ParseBitwiseExprConfig (ParseBitwiseExprConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseResult (ParseResult)

data ParseBitwiseExprConfig = ParseBitwiseExprConfig
  { parseBitwiseExprConfigNextParser :: [Token] -> Either ParseError (ParseResult Expr),
    parseBitwiseExprConfigTokens :: [Token]
  }
