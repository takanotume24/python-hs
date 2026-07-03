module PythonHS.Parser.ParseLambdaExprConfig (ParseLambdaExprConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)

data ParseLambdaExprConfig = ParseLambdaExprConfig
  { lambdaExprFallback :: [Token] -> Either ParseError (Expr, [Token]),
    lambdaExprTokens :: [Token]
  }
