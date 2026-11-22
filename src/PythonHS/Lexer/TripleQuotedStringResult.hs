module PythonHS.Lexer.TripleQuotedStringResult (TripleQuotedStringResult (..)) where

-- | Result of scanning a triple-quoted string literal.
data TripleQuotedStringResult = TripleQuotedStringResult
  { tripleQuotedStringResultContent :: String,
    tripleQuotedStringResultRemaining :: String,
    tripleQuotedStringResultLineCount :: Int,
    tripleQuotedStringResultEndColumn :: Int
  }
