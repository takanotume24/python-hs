{-# LANGUAGE NamedFieldPuns #-}
module PythonHS.VM.StmtPosition (stmtPosition) where

import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.Lexer.Position (Position)

stmtPosition :: Stmt -> Position
stmtPosition stmt =
  case stmt of
    AssignStmt { assignStmtPos } -> assignStmtPos
    AssignUnpackStmt { assignUnpackStmtPos } -> assignUnpackStmtPos
    AnnAssignStmt { annAssignStmtPos } -> annAssignStmtPos
    DecoratedStmt { decoratedStmtPos } -> decoratedStmtPos
    YieldStmt { yieldStmtPos } -> yieldStmtPos
    YieldFromStmt { yieldFromStmtPos } -> yieldFromStmtPos
    AddAssignStmt { addAssignStmtPos } -> addAssignStmtPos
    SubAssignStmt { subAssignStmtPos } -> subAssignStmtPos
    MulAssignStmt { mulAssignStmtPos } -> mulAssignStmtPos
    DivAssignStmt { divAssignStmtPos } -> divAssignStmtPos
    ModAssignStmt { modAssignStmtPos } -> modAssignStmtPos
    FloorDivAssignStmt { floorDivAssignStmtPos } -> floorDivAssignStmtPos
    FunctionDefStmt { functionDefStmtPos } -> functionDefStmtPos
    FunctionDefDefaultsStmt { functionDefDefaultsStmtPos } -> functionDefDefaultsStmtPos
    BreakStmt { breakStmtPos } -> breakStmtPos
    ContinueStmt { continueStmtPos } -> continueStmtPos
    ForStmt { forStmtPos } -> forStmtPos
    ClassDefStmt { classDefStmtPos } -> classDefStmtPos
    GlobalStmt { globalStmtPos } -> globalStmtPos
    ImportStmt { importStmtPos } -> importStmtPos
    FromImportStmt { fromImportStmtPos } -> fromImportStmtPos
    TryExceptStmt { tryExceptStmtPos } -> tryExceptStmtPos
    MatchStmt { matchStmtPos } -> matchStmtPos
    RaiseStmt { raiseStmtPos } -> raiseStmtPos
    IfStmt { ifStmtPos } -> ifStmtPos
    PassStmt { passStmtPos } -> passStmtPos
    ReturnStmt { returnStmtPos } -> returnStmtPos
    WhileStmt { whileStmtPos } -> whileStmtPos
    PrintStmt { printStmtPos } -> printStmtPos
    WithStmt { withStmtPos } -> withStmtPos
