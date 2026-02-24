module PythonHS.Parser.ParseError (ParseError (..)) where

import PythonHS.Lexer.Position (Position)

data ParseError
  = ExpectedExpression Position
  | ExpectedAssignAfterIdentifier Position
  | ExpectedNewlineAfterStatement Position
  deriving (Eq, Show)
