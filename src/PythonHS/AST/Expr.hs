module PythonHS.AST.Expr (Expr (..)) where

import PythonHS.AST.BinaryOperator (BinaryOperator)
import PythonHS.Lexer.Position (Position)

-- Expressions in the language with source position attached to each node
data Expr
  = IntegerExpr
      { integerExprValue :: Integer,
        integerExprPos :: Position
      }
  | FloatExpr
      { floatExprValue :: Double,
        floatExprPos :: Position
      }
  | StringExpr
      { stringExprValue :: String,
        stringExprPos :: Position
      }
  | NoneExpr {noneExprPos :: Position}
  | ListExpr
      { listExprItems :: [Expr],
        listExprPos :: Position
      }
  | TupleExpr
      { tupleExprItems :: [Expr],
        tupleExprPos :: Position
      }
  | ListComprehensionExpr
      { listComprehensionExprValue :: Expr,
        listComprehensionExprLoopName :: String,
        listComprehensionExprIter :: Expr,
        listComprehensionExprPos :: Position
      }
  | ListComprehensionClausesExpr
      { listComprehensionClausesExprValue :: Expr,
        listComprehensionClausesExprClauses :: [([String], Expr, [Expr])],
        listComprehensionClausesExprPos :: Position
      }
  | DictExpr
      { dictExprEntries :: [(Expr, Expr)],
        dictExprPos :: Position
      }
  | IdentifierExpr
      { identifierExprName :: String,
        identifierExprPos :: Position
      }
  | KeywordArgExpr
      { keywordArgExprName :: String,
        keywordArgExprValue :: Expr,
        keywordArgExprPos :: Position
      }
  | StarArgExpr
      { starArgExprValue :: Expr,
        starArgExprPos :: Position
      }
  | KwStarArgExpr
      { kwStarArgExprValue :: Expr,
        kwStarArgExprPos :: Position
      }
  | WalrusExpr
      { walrusExprName :: String,
        walrusExprValue :: Expr,
        walrusExprPos :: Position
      }
  | LambdaExpr
      { lambdaExprParams :: [String],
        lambdaExprValue :: Expr,
        lambdaExprPos :: Position
      }
  | LambdaDefaultsExpr
      { lambdaDefaultsExprParams :: [String],
        lambdaDefaultsExprDefaults :: [(String, Expr)],
        lambdaDefaultsExprValue :: Expr,
        lambdaDefaultsExprPos :: Position
      }
  | UnaryMinusExpr
      { unaryMinusExprValue :: Expr,
        unaryMinusExprPos :: Position
      }
  | NotExpr
      { notExprValue :: Expr,
        notExprPos :: Position
      }
  | BinaryExpr
      { binaryExprOp :: BinaryOperator,
        binaryExprLeft :: Expr,
        binaryExprRight :: Expr,
        binaryExprPos :: Position
      }
  | CallExpr
      { callExprName :: String,
        callExprArgs :: [Expr],
        callExprPos :: Position
      }
  | CallValueExpr
      { callValueExprCallee :: Expr,
        callValueExprArgs :: [Expr],
        callValueExprPos :: Position
      }
  | IndexExpr
      { indexExprBase :: Expr,
        indexExprIndex :: Expr,
        indexExprPos :: Position
      }
  | SliceExpr
      { sliceExprBase :: Expr,
        sliceExprStart :: Maybe Expr,
        sliceExprEnd :: Maybe Expr,
        sliceExprPos :: Position
      }
  deriving (Eq, Show)
