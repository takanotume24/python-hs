module PythonHS.Parser.ParseExprConfig (ParseExprConfig (..)) where

import PythonHS.Lexer.Token (Token)

data ParseExprConfig = ParseExprConfig
  { parseExprConfigAllowBitwiseOr :: Bool,
    parseExprConfigTokens :: [Token]
  }
