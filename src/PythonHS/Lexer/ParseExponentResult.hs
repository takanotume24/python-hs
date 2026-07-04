module PythonHS.Lexer.ParseExponentResult (ParseExponentResult (..)) where

data ParseExponentResult = ParseExponentResult
  { parseExponentResultDigits :: String,
    parseExponentResultRemaining :: String
  }
  deriving (Eq, Show)
