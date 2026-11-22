module PythonHS.Lexer.QuotedStringResult (QuotedStringResult (..)) where

-- | Raw content and remaining input for a single-quoted string literal.
data QuotedStringResult = QuotedStringResult
  { quotedStringContent :: String,
    quotedStringRemaining :: String
  }
