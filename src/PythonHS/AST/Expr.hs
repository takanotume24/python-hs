module PythonHS.AST.Expr (Expr (..)) where

import PythonHS.AST.BinaryOperator (BinaryOperator)
import PythonHS.Lexer.Position (Position)

-- Expressions in the language with source position attached to each node
data Expr
  = IntegerExpr Integer Position
  | FloatExpr Double Position
  | StringExpr String Position
  | NoneExpr Position
  | ListExpr [Expr] Position
  | ListComprehensionExpr Expr String Expr Position
  | ListComprehensionClausesExpr Expr [(String, Expr, Maybe Expr)] Position
  | DictExpr [(Expr, Expr)] Position
  | IdentifierExpr String Position
  | KeywordArgExpr String Expr Position
  | LambdaExpr [String] Expr Position
  | LambdaDefaultsExpr [String] [(String, Expr)] Expr Position
  | UnaryMinusExpr Expr Position
  | NotExpr Expr Position
  | BinaryExpr BinaryOperator Expr Expr Position
  | CallExpr String [Expr] Position -- function call: name(args...)
  | CallValueExpr Expr [Expr] Position
  deriving (Eq, Show)
