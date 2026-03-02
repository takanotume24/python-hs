module PythonHS.VM.HasYieldInStmts (hasYieldInStmts) where

import PythonHS.AST.Stmt (Stmt (ClassDefStmt, DecoratedStmt, ForStmt, FunctionDefDefaultsStmt, FunctionDefStmt, IfStmt, TryExceptStmt, WhileStmt, YieldFromStmt, YieldStmt))

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
        WhileStmt _ body _ -> hasYieldInStmts body || hasYieldInStmts rest
        ForStmt _ _ body _ -> hasYieldInStmts body || hasYieldInStmts rest
        TryExceptStmt tryStmts exceptSuites maybeFinally _ ->
          hasYieldInStmts tryStmts
            || any hasYieldInStmts exceptSuites
            || maybe False hasYieldInStmts maybeFinally
            || hasYieldInStmts rest
        DecoratedStmt _ innerStmt _ -> hasYieldInStmts [innerStmt] || hasYieldInStmts rest
        FunctionDefStmt _ _ _ _ -> hasYieldInStmts rest
        FunctionDefDefaultsStmt _ _ _ _ _ -> hasYieldInStmts rest
        ClassDefStmt _ _ _ _ -> hasYieldInStmts rest
        _ -> hasYieldInStmts rest
