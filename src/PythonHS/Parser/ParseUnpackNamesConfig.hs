module PythonHS.Parser.ParseUnpackNamesConfig (ParseUnpackNamesConfig (..)) where

import PythonHS.Lexer.Token (Token)

data ParseUnpackNamesConfig = ParseUnpackNamesConfig
  { unpackNamesAcc :: [String],
    unpackNamesTokens :: [Token]
  }
