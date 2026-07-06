module PythonHS.Parser.ParseModuleTailConfig (ParseModuleTailConfig (..)) where

import PythonHS.Lexer.Token (Token)

data ParseModuleTailConfig = ParseModuleTailConfig
  { parseModuleTailSegments :: [String],
    parseModuleTailTokenStream :: [Token]
  }
