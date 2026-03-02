module PythonHS.Parser.ExprPos (exprPos) where

import PythonHS.AST.Expr (Expr (BinaryExpr, CallExpr, DictExpr, FloatExpr, IdentifierExpr, IntegerExpr, KeywordArgExpr, LambdaExpr, ListComprehensionExpr, ListExpr, NoneExpr, NotExpr, StringExpr, UnaryMinusExpr))
import PythonHS.Lexer.Position (Position)

exprPos :: Expr -> Position
exprPos expr =
  case expr of
    IntegerExpr _ pos -> pos
    FloatExpr _ pos -> pos
    StringExpr _ pos -> pos
    NoneExpr pos -> pos
    ListExpr _ pos -> pos
    ListComprehensionExpr _ _ _ pos -> pos
    DictExpr _ pos -> pos
    IdentifierExpr _ pos -> pos
    KeywordArgExpr _ _ pos -> pos
    LambdaExpr _ _ pos -> pos
    UnaryMinusExpr _ pos -> pos
    NotExpr _ pos -> pos
    BinaryExpr _ _ _ pos -> pos
    CallExpr _ _ pos -> pos
