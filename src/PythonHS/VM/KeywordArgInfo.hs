module PythonHS.VM.KeywordArgInfo (KeywordArgInfo (..)) where

import PythonHS.Lexer.Position (Position)

data KeywordArgInfo = KeywordArgInfo
  { keywordArgInfoName :: String,
    keywordArgInfoPos :: Position
  }
  deriving (Eq, Show)
