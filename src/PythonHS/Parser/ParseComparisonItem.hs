module PythonHS.Parser.ParseComparisonItem (ParseComparisonItem (..)) where

import PythonHS.AST.BinaryOperator (BinaryOperator)
import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Position (Position)

data ParseComparisonItem = ParseComparisonItem
  { parseComparisonItemOp :: BinaryOperator,
    parseComparisonItemExpr :: Expr,
    parseComparisonItemPos :: Position
  }
