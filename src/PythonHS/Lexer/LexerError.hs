module PythonHS.Lexer.LexerError (LexerError (..)) where

newtype LexerError = UnexpectedCharacter {unexpectedChar :: Char}
  deriving (Eq)

instance Show LexerError where
  show (UnexpectedCharacter c) = "UnexpectedCharacter " ++ show c
