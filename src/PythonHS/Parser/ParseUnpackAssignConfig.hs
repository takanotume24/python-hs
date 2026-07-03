module PythonHS.Parser.ParseUnpackAssignConfig (ParseUnpackAssignConfig (..)) where

import PythonHS.Lexer.Position (Position)

data ParseUnpackAssignConfig = ParseUnpackAssignConfig
  { parseUnpackAssignFirstName :: String,
    parseUnpackAssignPos :: Position
  }
