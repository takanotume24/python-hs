module PythonHS.Lexer.Position (Position (..)) where

-- Source position (1-based line and column)
data Position = Position
  { line :: Int,
    column :: Int
  }
  deriving (Eq, Show)
