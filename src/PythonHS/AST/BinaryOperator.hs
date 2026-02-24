module PythonHS.AST.BinaryOperator (BinaryOperator (..)) where

data BinaryOperator
  = AddOperator
  | MultiplyOperator
  | DivideOperator
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
