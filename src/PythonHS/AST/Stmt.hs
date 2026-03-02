module PythonHS.AST.Stmt (Stmt (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Pattern (Pattern)
import PythonHS.Lexer.Position (Position)

-- Statements: assignment, print, control flow, function definition
-- each statement carries a source position for better error reporting
data Stmt
  = AssignStmt String Expr Position
  | AssignUnpackStmt [String] Expr Position
  | AnnAssignStmt String Expr (Maybe Expr) Position -- name, annotation, optional value
  | DecoratedStmt [Expr] Stmt Position
  | YieldStmt Expr Position
  | YieldFromStmt Expr Position
  | AddAssignStmt String Expr Position
  | SubAssignStmt String Expr Position
  | MulAssignStmt String Expr Position
  | DivAssignStmt String Expr Position
  | ModAssignStmt String Expr Position
  | FloorDivAssignStmt String Expr Position
  | PrintStmt Expr Position
  | ReturnStmt Expr Position
  | BreakStmt Position
  | ContinueStmt Position
  | GlobalStmt String Position
  | ImportStmt [([String], Maybe String)] Position
  | FromImportStmt [String] [(String, Maybe String)] Position
  | TryExceptStmt [Stmt] [[Stmt]] (Maybe [Stmt]) Position
  | MatchStmt Expr [(Pattern, Maybe Expr, [Stmt], Position)] Position
  | RaiseStmt Expr Position
  | PassStmt Position
  | IfStmt Expr [Stmt] (Maybe [Stmt]) Position      -- condition, then-branch, optional else-branch
  | WhileStmt Expr [Stmt] Position                  -- condition, body
  | ForStmt String Expr [Stmt] Position             -- loop variable, iterable expression, body
  | ClassDefStmt String (Maybe String) [Stmt] Position -- class name, optional base, body
  | FunctionDefStmt String [String] [Stmt] Position -- name, params, body
  | FunctionDefDefaultsStmt String [String] [(String, Expr)] [Stmt] Position -- name, params, defaults, body
  deriving (Eq, Show)
