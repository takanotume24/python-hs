module PythonHS.Parser.ParsePowerExprConfig (ParsePowerExprConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseResult (ParseResult)

-- | Inputs for parsing a power expression, including unary prefixes.
data ParsePowerExprConfig = ParsePowerExprConfig
  { parsePowerExprConfigParsePrimary :: [Token] -> Either ParseError (ParseResult Expr),
    parsePowerExprConfigTokens :: [Token]
  }
