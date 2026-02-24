module PythonHS.Lexer.LexerError (LexerError (..)) where

data LexerError = UnexpectedCharacter Char
  deriving (Eq, Show)
