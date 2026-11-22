module PythonHS.AST.Stmt (Stmt (..)) where

import PythonHS.AST.ExceptSuite (ExceptSuite)
import PythonHS.AST.Expr (Expr)
import PythonHS.AST.FromImportEntry (FromImportEntry)
import PythonHS.AST.FunctionDefault (FunctionDefault)
import PythonHS.AST.ImportEntry (ImportEntry)
import PythonHS.AST.MatchCase (MatchCase)
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
  | SubscriptAssignStmt
      { subscriptAssignStmtTarget :: Expr,
        subscriptAssignStmtValue :: Expr,
        subscriptAssignStmtPos :: Position
      }
  | DeleteStmt
      { deleteStmtTarget :: String,
        deleteStmtPos :: Position
      }
  | DeleteSubscriptStmt
      { deleteSubscriptStmtTarget :: Expr,
        deleteSubscriptStmtPos :: Position
      }
  | AssertStmt
      { assertStmtCond :: Expr,
        assertStmtPos :: Position
      }
  | ExprStmt
      { exprStmtValue :: Expr,
        exprStmtPos :: Position
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
  | NonlocalStmt
      { nonlocalStmtName :: String,
        nonlocalStmtPos :: Position
      }
  | ImportStmt
      { importStmtItems :: [ImportEntry],
        importStmtPos :: Position
      }
  | FromImportStmt
      { fromImportStmtLevel :: Int,
        fromImportStmtModule :: [String],
        fromImportStmtItems :: [FromImportEntry],
        fromImportStmtPos :: Position
      }
  | TryExceptStmt
      { tryExceptStmtTryBody :: [Stmt],
        tryExceptStmtExceptSuites :: [ExceptSuite Stmt],
        tryExceptStmtElseBody :: Maybe [Stmt],
        tryExceptStmtFinallyBody :: Maybe [Stmt],
        tryExceptStmtPos :: Position
      }
  | MatchStmt
      { matchStmtSubject :: Expr,
        matchStmtCases :: [MatchCase Expr Pattern Stmt],
        matchStmtPos :: Position
      }
  | RaiseStmt
      { raiseStmtExpr :: Maybe Expr,
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
        functionDefDefaultsStmtDefaults :: [FunctionDefault Expr],
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
