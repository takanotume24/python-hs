module PythonHS.Parser.ParseSubscriptExprConfig (ParseSubscriptExprConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Position (Position)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseResult (ParseResult)

data ParseSubscriptExprConfig = ParseSubscriptExprConfig
  { parseSubscriptExprFn :: [Token] -> Either ParseError (ParseResult Expr),
    parseSubscriptExprReceiver :: Expr,
    parseSubscriptExprPos :: Position,
    parseSubscriptExprTokenStream :: [Token]
  }
