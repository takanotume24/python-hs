module PythonHS.Parser.ParseError (ParseError (..)) where

import PythonHS.Lexer.Position (Position)

data ParseError
  = ExpectedExpression {parseErrorPosition :: Position}
  | ExpectedAssignAfterIdentifier {parseErrorPosition :: Position}
  | ExpectedNewlineAfterStatement {parseErrorPosition :: Position}
  deriving (Eq)

instance Show ParseError where
  show (ExpectedExpression pos) = "ExpectedExpression (" ++ show pos ++ ")"
  show (ExpectedAssignAfterIdentifier pos) = "ExpectedAssignAfterIdentifier (" ++ show pos ++ ")"
  show (ExpectedNewlineAfterStatement pos) = "ExpectedNewlineAfterStatement (" ++ show pos ++ ")"
