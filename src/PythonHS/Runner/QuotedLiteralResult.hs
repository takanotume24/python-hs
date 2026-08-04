module PythonHS.Runner.QuotedLiteralResult (QuotedLiteralResult (..)) where

data QuotedLiteralResult = QuotedLiteralResult
  { quotedLiteralResultLiteral :: String,
    quotedLiteralResultRest :: String
  }
  deriving (Eq, Show)
