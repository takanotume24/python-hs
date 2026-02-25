module PythonHS.AST.BinaryOperator (BinaryOperator (..)) where

data BinaryOperator
  = AddOperator
  | SubtractOperator
  | MultiplyOperator
  | DivideOperator
  | FloorDivideOperator
  | ModuloOperator
  | EqOperator
  | NotEqOperator
  | LtOperator
  | GtOperator
  | LteOperator
  | GteOperator
  | AndOperator
  | OrOperator
  deriving (Eq, Show)
