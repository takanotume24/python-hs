module PythonHS.Parser.ParseLeadingDotsConfig (ParseLeadingDotsConfig (..)) where

import PythonHS.Lexer.Token (Token)

data ParseLeadingDotsConfig = ParseLeadingDotsConfig
  { parseLeadingDotsCount :: Int,
    parseLeadingDotsTokenStream :: [Token]
  }
