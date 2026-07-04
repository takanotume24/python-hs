module PythonHS.Parser.ParseSubscriptExprConfig (ParseSubscriptExprConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Position (Position)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)

data ParseSubscriptExprConfig = ParseSubscriptExprConfig
  { parseSubscriptExprFn :: [Token] -> Either ParseError (Expr, [Token]),
    parseSubscriptExprReceiver :: Expr,
    parseSubscriptExprPos :: Position,
    parseSubscriptExprTokenStream :: [Token]
  }
