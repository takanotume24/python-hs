module PythonHS.Parser.ParseUnpackAssignConfig (ParseUnpackAssignConfig (..)) where

import PythonHS.Lexer.Position (Position)
import PythonHS.Lexer.Token (Token)

data ParseUnpackAssignConfig = ParseUnpackAssignConfig
  { parseUnpackAssignFirstName :: String,
    parseUnpackAssignPos :: Position,
    parseUnpackAssignTokenStream :: [Token]
  }
