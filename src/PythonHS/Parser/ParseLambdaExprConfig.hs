module PythonHS.Parser.ParseLambdaExprConfig (ParseLambdaExprConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseResult (ParseResult)

data ParseLambdaExprConfig = ParseLambdaExprConfig
  { lambdaExprFallback :: [Token] -> Either ParseError (ParseResult Expr),
    lambdaExprTokens :: [Token]
  }
