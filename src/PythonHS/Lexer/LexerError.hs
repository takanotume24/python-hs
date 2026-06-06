module PythonHS.Lexer.LexerError (LexerError (..)) where

data LexerError = UnexpectedCharacter { unexpectedChar :: Char }
  deriving (Eq)

instance Show LexerError where
  show (UnexpectedCharacter c) = "UnexpectedCharacter " ++ show c
