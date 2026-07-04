module PythonHS.Lexer.AdjustIndentResult (AdjustIndentResult (..)) where

import PythonHS.Lexer.Token (Token)

data AdjustIndentResult = AdjustIndentResult
  { adjustIndentResultStack :: [Int],
    adjustIndentResultTokens :: [Token]
  }
  deriving (Eq, Show)
