module PythonHS.VM.CollectFunctionGlobalStmtDecls (collectFunctionGlobalStmtDecls) where

import Data.Set (Set)
import Data.Set qualified as Set
import PythonHS.AST.ExceptSuite (ExceptSuite (..))
import PythonHS.AST.MatchCase (MatchCase (..))
import PythonHS.AST.Stmt (Stmt (..))

collectFunctionGlobalStmtDecls :: [Stmt] -> Set String
collectFunctionGlobalStmtDecls stmts =
  foldMap collectFromStmt stmts
  where
    collectFromStmt stmt =
      case stmt of
        GlobalStmt {globalStmtName = name} -> Set.singleton name
        IfStmt {ifStmtThen = thenBranch, ifStmtElse = maybeElse} ->
          let fromThen = foldMap collectFromStmt thenBranch
              fromElse = maybe Set.empty (foldMap collectFromStmt) maybeElse
           in Set.union fromThen fromElse
        WhileStmt {whileStmtBody = body} -> foldMap collectFromStmt body
        ForStmt {forStmtBody = body} -> foldMap collectFromStmt body
        TryExceptStmt {tryExceptStmtTryBody = tryBody, tryExceptStmtExceptSuites = exceptSuites, tryExceptStmtElseBody = maybeElse, tryExceptStmtFinallyBody = maybeFinally} ->
          let fromTry = foldMap collectFromStmt tryBody
              fromElse = maybe Set.empty (foldMap collectFromStmt) maybeElse
              fromExcept = foldMap (foldMap collectFromStmt . exceptSuiteBody) exceptSuites
              fromFinally = maybe Set.empty (foldMap collectFromStmt) maybeFinally
           in Set.unions [fromTry, fromExcept, fromFinally, fromElse]
        WithStmt {withStmtBody = body} -> foldMap collectFromStmt body
        MatchStmt {matchStmtCases = cases} ->
          foldMap (foldMap collectFromStmt . matchCaseBody) cases
        DecoratedStmt {decoratedStmtTarget = innerStmt} -> collectFromStmt innerStmt
        _ -> Set.empty
