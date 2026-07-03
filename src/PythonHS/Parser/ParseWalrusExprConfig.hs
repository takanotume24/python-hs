module PythonHS.Parser.ParseWalrusExprConfig (ParseWalrusExprConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)

data ParseWalrusExprConfig = ParseWalrusExprConfig
  { walrusExprFallback :: [Token] -> Either ParseError (Expr, [Token]),
    walrusExprTokens :: [Token]
  }
