module PythonHS.VM.HasYieldInStmts (hasYieldInStmts) where

import PythonHS.AST.ExceptSuite (ExceptSuite (..))
import PythonHS.AST.Stmt (Stmt (..))

hasYieldInStmts :: [Stmt] -> Bool
hasYieldInStmts stmts =
  case stmts of
    [] -> False
    stmt : rest ->
      case stmt of
        YieldStmt _ _ -> True
        YieldFromStmt _ _ -> True
        IfStmt _ thenStmts maybeElseStmts _ ->
          hasYieldInStmts thenStmts || maybe False hasYieldInStmts maybeElseStmts || hasYieldInStmts rest
        WhileStmt _ body maybeElse _ -> hasYieldInStmts body || maybe False hasYieldInStmts maybeElse || hasYieldInStmts rest
        ForStmt _ _ body maybeElse _ -> hasYieldInStmts body || maybe False hasYieldInStmts maybeElse || hasYieldInStmts rest
        TryExceptStmt {tryExceptStmtTryBody = tryStmts, tryExceptStmtExceptSuites = exceptSuites, tryExceptStmtElseBody = maybeElse, tryExceptStmtFinallyBody = maybeFinally} ->
          hasYieldInStmts tryStmts
            || any (\ExceptSuite {exceptSuiteBody = suiteStmts} -> hasYieldInStmts suiteStmts) exceptSuites
            || maybe False hasYieldInStmts maybeElse
            || maybe False hasYieldInStmts maybeFinally
            || hasYieldInStmts rest
        DecoratedStmt _ innerStmt _ -> hasYieldInStmts [innerStmt] || hasYieldInStmts rest
        FunctionDefStmt _ _ _ _ -> hasYieldInStmts rest
        FunctionDefDefaultsStmt _ _ _ _ _ -> hasYieldInStmts rest
        ClassDefStmt _ _ _ _ -> hasYieldInStmts rest
        _ -> hasYieldInStmts rest
