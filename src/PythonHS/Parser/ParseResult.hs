module PythonHS.Parser.ParseResult (ParseResult (..)) where

import PythonHS.Lexer.Token (Token)

data ParseResult a = ParseResult
  { parseResultValue :: a,
    parseResultRemaining :: [Token]
  }
  deriving (Eq, Show)
