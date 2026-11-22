module PythonHS.Parser.ParseConditionalExprConfig (ParseConditionalExprConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Token (Token)
import PythonHS.Parser.ParseError (ParseError)
import PythonHS.Parser.ParseResult (ParseResult)

data ParseConditionalExprConfig = ParseConditionalExprConfig
  { parseConditionalExprBase :: [Token] -> Either ParseError (ParseResult Expr),
    parseConditionalExprCondition :: [Token] -> Either ParseError (ParseResult Expr),
    parseConditionalExprTokens :: [Token]
  }
