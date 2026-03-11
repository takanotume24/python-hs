module PythonHS.VM.StmtPosition (stmtPosition) where

import PythonHS.AST.Stmt (Stmt (AddAssignStmt, AnnAssignStmt, AssignStmt, AssignUnpackStmt, BreakStmt, ClassDefStmt, ContinueStmt, DecoratedStmt, DivAssignStmt, FloorDivAssignStmt, ForStmt, FromImportStmt, FunctionDefDefaultsStmt, FunctionDefStmt, GlobalStmt, IfStmt, ImportStmt, MatchStmt, ModAssignStmt, MulAssignStmt, PassStmt, PrintStmt, RaiseStmt, ReturnStmt, SubAssignStmt, TryExceptStmt, WhileStmt, YieldFromStmt, YieldStmt))
import PythonHS.Lexer.Position (Position)

stmtPosition :: Stmt -> Position
stmtPosition stmt =
  case stmt of
    AssignStmt _ _ pos -> pos
    AssignUnpackStmt _ _ pos -> pos
    AnnAssignStmt _ _ _ pos -> pos
    DecoratedStmt _ _ pos -> pos
    YieldStmt _ pos -> pos
    YieldFromStmt _ pos -> pos
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
    ClassDefStmt _ _ _ pos -> pos
    GlobalStmt _ pos -> pos
    ImportStmt _ pos -> pos
    FromImportStmt _ _ _ pos -> pos
    TryExceptStmt _ _ _ pos -> pos
    MatchStmt _ _ pos -> pos
    RaiseStmt _ pos -> pos
    IfStmt _ _ _ pos -> pos
    PassStmt pos -> pos
    ReturnStmt _ pos -> pos
    WhileStmt _ _ pos -> pos
    PrintStmt _ pos -> pos
