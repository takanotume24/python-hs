module PythonHS.Lexer.Token (Token (..)) where

import PythonHS.Lexer.Position (Position (..))
import PythonHS.Lexer.TokenType (TokenType)

data Token = Token
  { tokenType :: TokenType,
    lexeme :: String,
    position :: Position
  }
  deriving (Eq, Show)
