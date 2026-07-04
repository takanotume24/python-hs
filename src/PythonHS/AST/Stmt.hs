module PythonHS.AST.Stmt (Stmt (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Pattern (Pattern)
import PythonHS.Lexer.Position (Position)

-- Statements: assignment, print, control flow, function definition
-- each statement carries a source position for better error reporting
data Stmt
  = AssignStmt
      { assignStmtName :: String,
        assignStmtValue :: Expr,
        assignStmtPos :: Position
      }
  | AssignUnpackStmt
      { assignUnpackStmtNames :: [String],
        assignUnpackStmtValue :: Expr,
        assignUnpackStmtPos :: Position
      }
  | AnnAssignStmt
      { annAssignStmtName :: String,
        annAssignStmtAnnotation :: Expr,
        annAssignStmtValue :: Maybe Expr,
        annAssignStmtPos :: Position
      }
  | DecoratedStmt
      { decoratedStmtDecorators :: [Expr],
        decoratedStmtTarget :: Stmt,
        decoratedStmtPos :: Position
      }
  | YieldStmt
      { yieldStmtValue :: Expr,
        yieldStmtPos :: Position
      }
  | YieldFromStmt
      { yieldFromStmtValue :: Expr,
        yieldFromStmtPos :: Position
      }
  | AddAssignStmt
      { addAssignStmtName :: String,
        addAssignStmtValue :: Expr,
        addAssignStmtPos :: Position
      }
  | SubAssignStmt
      { subAssignStmtName :: String,
        subAssignStmtValue :: Expr,
        subAssignStmtPos :: Position
      }
  | MulAssignStmt
      { mulAssignStmtName :: String,
        mulAssignStmtValue :: Expr,
        mulAssignStmtPos :: Position
      }
  | DivAssignStmt
      { divAssignStmtName :: String,
        divAssignStmtValue :: Expr,
        divAssignStmtPos :: Position
      }
  | ModAssignStmt
      { modAssignStmtName :: String,
        modAssignStmtValue :: Expr,
        modAssignStmtPos :: Position
      }
  | FloorDivAssignStmt
      { floorDivAssignStmtName :: String,
        floorDivAssignStmtValue :: Expr,
        floorDivAssignStmtPos :: Position
      }
  | PrintStmt
      { printStmtValue :: Expr,
        printStmtPos :: Position
      }
  | ReturnStmt
      { returnStmtValue :: Expr,
        returnStmtPos :: Position
      }
  | BreakStmt {breakStmtPos :: Position}
  | ContinueStmt {continueStmtPos :: Position}
  | GlobalStmt
      { globalStmtName :: String,
        globalStmtPos :: Position
      }
  | ImportStmt
      { importStmtItems :: [([String], Maybe String)],
        importStmtPos :: Position
      }
  | FromImportStmt
      { fromImportStmtLevel :: Int,
        fromImportStmtModule :: [String],
        fromImportStmtItems :: [(String, Maybe String)],
        fromImportStmtPos :: Position
      }
  | TryExceptStmt
      { tryExceptStmtTryBody :: [Stmt],
        tryExceptStmtExceptSuites :: [(Maybe String, Maybe String, [Stmt], Position)],
        tryExceptStmtFinallyBody :: Maybe [Stmt],
        tryExceptStmtPos :: Position
      }
  | MatchStmt
      { matchStmtSubject :: Expr,
        matchStmtCases :: [(Pattern, Maybe Expr, [Stmt], Position)],
        matchStmtPos :: Position
      }
  | RaiseStmt
      { raiseStmtExpr :: Expr,
        raiseStmtPos :: Position
      }
  | PassStmt {passStmtPos :: Position}
  | IfStmt
      { ifStmtCond :: Expr,
        ifStmtThen :: [Stmt],
        ifStmtElse :: Maybe [Stmt],
        ifStmtPos :: Position
      }
  | WhileStmt
      { whileStmtCond :: Expr,
        whileStmtBody :: [Stmt],
        whileStmtPos :: Position
      }
  | ForStmt
      { forStmtVar :: String,
        forStmtIter :: Expr,
        forStmtBody :: [Stmt],
        forStmtPos :: Position
      }
  | ClassDefStmt
      { classDefStmtName :: String,
        classDefStmtBase :: Maybe String,
        classDefStmtBody :: [Stmt],
        classDefStmtPos :: Position
      }
  | FunctionDefStmt
      { functionDefStmtName :: String,
        functionDefStmtParams :: [String],
        functionDefStmtBody :: [Stmt],
        functionDefStmtPos :: Position
      }
  | FunctionDefDefaultsStmt
      { functionDefDefaultsStmtName :: String,
        functionDefDefaultsStmtParams :: [String],
        functionDefDefaultsStmtDefaults :: [(String, Expr)],
        functionDefDefaultsStmtBody :: [Stmt],
        functionDefDefaultsStmtPos :: Position
      }
  | WithStmt
      { withStmtContextManager :: Expr,
        withStmtVarName :: Maybe String,
        withStmtBody :: [Stmt],
        withStmtPos :: Position
      }
  deriving (Eq, Show)
