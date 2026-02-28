module PythonHS.AST.Expr (Expr (..)) where

import PythonHS.AST.BinaryOperator (BinaryOperator)
import PythonHS.Lexer.Position (Position)

-- Expressions in the language with source position attached to each node
data Expr
  = IntegerExpr Int Position
  | StringExpr String Position
  | NoneExpr Position
  | ListExpr [Expr] Position
  | DictExpr [(Expr, Expr)] Position
  | IdentifierExpr String Position
  | KeywordArgExpr String Expr Position
  | UnaryMinusExpr Expr Position
  | NotExpr Expr Position
  | BinaryExpr BinaryOperator Expr Expr Position
  | CallExpr String [Expr] Position -- function call: name(args...)
  deriving (Eq, Show)
