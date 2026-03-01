module PythonHS.VM.StmtPosition (stmtPosition) where

import PythonHS.AST.Stmt (Stmt (AddAssignStmt, AssignStmt, BreakStmt, ContinueStmt, DivAssignStmt, FloorDivAssignStmt, ForStmt, FunctionDefDefaultsStmt, FunctionDefStmt, GlobalStmt, IfStmt, ModAssignStmt, MulAssignStmt, PassStmt, PrintStmt, ReturnStmt, SubAssignStmt, WhileStmt))
import PythonHS.Lexer.Position (Position)

stmtPosition :: Stmt -> Position
stmtPosition stmt =
  case stmt of
    AssignStmt _ _ pos -> pos
    AddAssignStmt _ _ pos -> pos
    SubAssignStmt _ _ pos -> pos
    MulAssignStmt _ _ pos -> pos
    DivAssignStmt _ _ pos -> pos
    ModAssignStmt _ _ pos -> pos
    FloorDivAssignStmt _ _ pos -> pos
    FunctionDefStmt _ _ _ pos -> pos
    FunctionDefDefaultsStmt _ _ _ _ pos -> pos
    BreakStmt pos -> pos
    ContinueStmt pos -> pos
    ForStmt _ _ _ pos -> pos
    GlobalStmt _ pos -> pos
    IfStmt _ _ _ pos -> pos
    PassStmt pos -> pos
    ReturnStmt _ pos -> pos
    WhileStmt _ _ pos -> pos
    PrintStmt _ pos -> pos
