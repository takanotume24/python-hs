module PythonHS.AST.MatchCase (MatchCase (..)) where

import PythonHS.Lexer.Position (Position)

data MatchCase e p s = MatchCase
  { matchCasePattern :: p,
    matchCaseGuard :: Maybe e,
    matchCaseBody :: [s],
    matchCasePos :: Position
  }
  deriving (Eq, Show)
