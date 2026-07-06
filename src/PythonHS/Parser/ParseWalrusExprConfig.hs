module PythonHS.Parser.ParseWalrusExprConfig (ParseWalrusExprConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseResult (ParseResult)

data ParseWalrusExprConfig = ParseWalrusExprConfig
  { walrusExprFallback :: [Token] -> Either ParseError (ParseResult Expr),
    walrusExprTokens :: [Token]
  }
