module PythonHS.AST.Stmt (Stmt (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Position (Position)

-- Statements: assignment, print, control flow, function definition
-- each statement carries a source position for better error reporting
data Stmt
  = AssignStmt String Expr Position
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
  | PassStmt Position
  | IfStmt Expr [Stmt] (Maybe [Stmt]) Position      -- condition, then-branch, optional else-branch
  | WhileStmt Expr [Stmt] Position                  -- condition, body
  | ForStmt String Expr [Stmt] Position             -- loop variable, iterable expression, body
  | FunctionDefStmt String [String] [Stmt] Position -- name, params, body
  deriving (Eq, Show)
