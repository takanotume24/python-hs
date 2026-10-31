module PythonHS.VM.CollectFunctionLocalDecls (collectFunctionLocalDecls) where

import Data.Set (Set)
import Data.Set qualified as Set
import PythonHS.AST.ExceptSuite (ExceptSuite (..))
import PythonHS.AST.MatchCase (MatchCase (..))
import PythonHS.AST.Stmt (Stmt (..))

collectFunctionLocalDecls :: [String] -> [Stmt] -> Set String -> Set String
collectFunctionLocalDecls params stmts globalDecls =
  let collected = foldMap collectFromStmt stmts
   in Set.difference collected (Set.union (Set.fromList params) globalDecls)
  where
    collectFromStmt stmt =
      case stmt of
        AssignStmt {assignStmtName = name} -> addSimpleName name
        AddAssignStmt {addAssignStmtName = name} -> addSimpleName name
        SubAssignStmt {subAssignStmtName = name} -> addSimpleName name
        MulAssignStmt {mulAssignStmtName = name} -> addSimpleName name
        DivAssignStmt {divAssignStmtName = name} -> addSimpleName name
        ModAssignStmt {modAssignStmtName = name} -> addSimpleName name
        FloorDivAssignStmt {floorDivAssignStmtName = name} -> addSimpleName name
        AnnAssignStmt {annAssignStmtName = name} -> addSimpleName name
        AssignUnpackStmt {assignUnpackStmtNames = names} -> Set.fromList (filter (not . isAttrPath) names)
        DeleteStmt {deleteStmtTarget = target} -> addSimpleName target
        ForStmt {forStmtVar = name, forStmtBody = body} -> Set.insert name (foldMap collectFromStmt body)
        WithStmt {withStmtVarName = maybeName, withStmtBody = body} ->
          let fromName = maybe Set.empty Set.singleton maybeName
           in Set.union fromName (foldMap collectFromStmt body)
        IfStmt {ifStmtThen = thenBranch, ifStmtElse = maybeElse} ->
          let fromThen = foldMap collectFromStmt thenBranch
              fromElse = maybe Set.empty (foldMap collectFromStmt) maybeElse
           in Set.union fromThen fromElse
        WhileStmt {whileStmtBody = body} -> foldMap collectFromStmt body
        TryExceptStmt {tryExceptStmtTryBody = tryBody, tryExceptStmtExceptSuites = exceptSuites, tryExceptStmtFinallyBody = maybeFinally} ->
          let fromTry = foldMap collectFromStmt tryBody
              fromExcept = foldMap collectFromExceptSuite exceptSuites
              fromFinally = maybe Set.empty (foldMap collectFromStmt) maybeFinally
           in Set.unions [fromTry, fromExcept, fromFinally]
        MatchStmt {matchStmtCases = cases} ->
          foldMap (\caseItem -> foldMap collectFromStmt (matchCaseBody caseItem)) cases
        FunctionDefStmt {functionDefStmtName = name} -> Set.singleton name
        FunctionDefDefaultsStmt {functionDefDefaultsStmtName = name} -> Set.singleton name
        ClassDefStmt {classDefStmtName = name} -> Set.singleton name
        DecoratedStmt {decoratedStmtTarget = innerStmt} -> collectFromStmt innerStmt
        _ -> Set.empty

    collectFromExceptSuite suite =
      let fromAlias = maybe Set.empty Set.singleton (exceptSuiteAlias suite)
          fromBody = foldMap collectFromStmt (exceptSuiteBody suite)
       in Set.union fromAlias fromBody

    addSimpleName name =
      if isAttrPath name
        then Set.empty
        else Set.singleton name

    isAttrPath name = elem '.' name
