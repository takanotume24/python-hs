module PythonHS.Lexer.Token (Token (..)) where

import PythonHS.Lexer.TokenType (TokenType)
import PythonHS.Lexer.Position (Position (..))

data Token = Token
  { tokenType :: TokenType,
    lexeme :: String,
    position :: Position
  }
  deriving (Eq, Show)
