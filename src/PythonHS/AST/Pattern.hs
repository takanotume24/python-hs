module PythonHS.AST.Pattern (Pattern (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Position (Position)

data Pattern
  = ValuePattern Expr Position
  | WildcardPattern Position
  | CapturePattern String Position
  | OrPattern [Pattern] Position
  | SequencePattern [Pattern] (Maybe String) Position
  | MappingPattern [(Expr, Pattern)] Position
  deriving (Eq, Show)
