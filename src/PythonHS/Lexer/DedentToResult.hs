module PythonHS.Lexer.DedentToResult (DedentToResult (..)) where

import PythonHS.Lexer.Token (Token)

data DedentToResult = DedentToResult
  { dedentToResultStack :: [Int],
    dedentToResultTokens :: [Token]
  }
  deriving (Eq, Show)
