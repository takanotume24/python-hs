{-# LANGUAGE NamedFieldPuns #-}
module PythonHS.Parser.ExprPos (exprPos) where

import PythonHS.AST.Expr (Expr (..))
import PythonHS.Lexer.Position (Position)

exprPos :: Expr -> Position
exprPos expr =
  case expr of
    IntegerExpr { integerExprPos } -> integerExprPos
    FloatExpr { floatExprPos } -> floatExprPos
    StringExpr { stringExprPos } -> stringExprPos
    NoneExpr { noneExprPos } -> noneExprPos
    ListExpr { listExprPos } -> listExprPos
    TupleExpr { tupleExprPos } -> tupleExprPos
    ListComprehensionExpr { listComprehensionExprPos } -> listComprehensionExprPos
    ListComprehensionClausesExpr { listComprehensionClausesExprPos } -> listComprehensionClausesExprPos
    DictExpr { dictExprPos } -> dictExprPos
    IdentifierExpr { identifierExprPos } -> identifierExprPos
    KeywordArgExpr { keywordArgExprPos } -> keywordArgExprPos
    StarArgExpr { starArgExprPos } -> starArgExprPos
    KwStarArgExpr { kwStarArgExprPos } -> kwStarArgExprPos
    WalrusExpr { walrusExprPos } -> walrusExprPos
    LambdaExpr { lambdaExprPos } -> lambdaExprPos
    LambdaDefaultsExpr { lambdaDefaultsExprPos } -> lambdaDefaultsExprPos
    UnaryMinusExpr { unaryMinusExprPos } -> unaryMinusExprPos
    NotExpr { notExprPos } -> notExprPos
    BinaryExpr { binaryExprPos } -> binaryExprPos
    CallExpr { callExprPos } -> callExprPos
    CallValueExpr { callValueExprPos } -> callValueExprPos
    IndexExpr { indexExprPos } -> indexExprPos
    SliceExpr { sliceExprPos } -> sliceExprPos
