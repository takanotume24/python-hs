module PythonHS.VM.ExprPosition (exprPosition) where

import PythonHS.AST.Expr (Expr (BinaryExpr, CallExpr, CallValueExpr, DictExpr, FloatExpr, IdentifierExpr, IntegerExpr, KeywordArgExpr, KwStarArgExpr, LambdaDefaultsExpr, LambdaExpr, ListComprehensionClausesExpr, ListComprehensionExpr, ListExpr, NoneExpr, NotExpr, StarArgExpr, StringExpr, UnaryMinusExpr, WalrusExpr))
import PythonHS.Lexer.Position (Position)

exprPosition :: Expr -> Position
exprPosition expr =
  case expr of
    IntegerExpr _ pos -> pos
    FloatExpr _ pos -> pos
    StringExpr _ pos -> pos
    NoneExpr pos -> pos
    ListExpr _ pos -> pos
    ListComprehensionExpr _ _ _ pos -> pos
    ListComprehensionClausesExpr _ _ pos -> pos
    DictExpr _ pos -> pos
    IdentifierExpr _ pos -> pos
    KeywordArgExpr _ _ pos -> pos
    StarArgExpr _ pos -> pos
    KwStarArgExpr _ pos -> pos
    WalrusExpr _ _ pos -> pos
    LambdaExpr _ _ pos -> pos
    LambdaDefaultsExpr _ _ _ pos -> pos
    UnaryMinusExpr _ pos -> pos
    NotExpr _ pos -> pos
    BinaryExpr _ _ _ pos -> pos
    CallExpr _ _ pos -> pos
    CallValueExpr _ _ pos -> pos
