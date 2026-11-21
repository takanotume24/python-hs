module PythonHS.AST.BinaryOperator (BinaryOperator (..)) where

data BinaryOperator
  = AddOperator
  | SubtractOperator
  | MultiplyOperator
  | PowerOperator
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
  | InOperator
  | NotInOperator
  | IsOperator
  | IsNotOperator
  deriving (Eq, Show)
