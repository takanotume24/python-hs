module PythonHS.VM.TransformImportAliases (transformImportAliases) where

import qualified Data.Map.Strict as Map
import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.Stmt (Stmt (..))

transformImportAliases :: Bool -> Map.Map String String -> Map.Map String String -> Map.Map String String -> Stmt -> Stmt
transformImportAliases renameDefNames moduleAlias callAlias identAlias stmt =
  case stmt of
    AssignStmt {assignStmtName = name, assignStmtValue = expr, assignStmtPos = pos} ->
      AssignStmt (renameName renameDefNames callAlias name) (transformExpr moduleAlias callAlias identAlias expr) pos
    AssignUnpackStmt {assignUnpackStmtNames = names, assignUnpackStmtValue = expr, assignUnpackStmtPos = pos} ->
      AssignUnpackStmt (fmap (renameName renameDefNames callAlias) names) (transformExpr moduleAlias callAlias identAlias expr) pos
    AnnAssignStmt {annAssignStmtName = name, annAssignStmtAnnotation = annotation, annAssignStmtValue = maybeExpr, annAssignStmtPos = pos} ->
      AnnAssignStmt
        (renameName renameDefNames callAlias name)
        (transformExpr moduleAlias callAlias identAlias annotation)
        (fmap (transformExpr moduleAlias callAlias identAlias) maybeExpr)
        pos
    DecoratedStmt {decoratedStmtDecorators = decorators, decoratedStmtTarget = innerStmt, decoratedStmtPos = pos} ->
      DecoratedStmt
        (fmap (transformExpr moduleAlias callAlias identAlias) decorators)
        (transformImportAliases renameDefNames moduleAlias callAlias identAlias innerStmt)
        pos
    AddAssignStmt {addAssignStmtName = name, addAssignStmtValue = expr, addAssignStmtPos = pos} ->
      AddAssignStmt name (transformExpr moduleAlias callAlias identAlias expr) pos
    SubAssignStmt {subAssignStmtName = name, subAssignStmtValue = expr, subAssignStmtPos = pos} ->
      SubAssignStmt name (transformExpr moduleAlias callAlias identAlias expr) pos
    MulAssignStmt {mulAssignStmtName = name, mulAssignStmtValue = expr, mulAssignStmtPos = pos} ->
      MulAssignStmt name (transformExpr moduleAlias callAlias identAlias expr) pos
    DivAssignStmt {divAssignStmtName = name, divAssignStmtValue = expr, divAssignStmtPos = pos} ->
      DivAssignStmt name (transformExpr moduleAlias callAlias identAlias expr) pos
    ModAssignStmt {modAssignStmtName = name, modAssignStmtValue = expr, modAssignStmtPos = pos} ->
      ModAssignStmt name (transformExpr moduleAlias callAlias identAlias expr) pos
    FloorDivAssignStmt {floorDivAssignStmtName = name, floorDivAssignStmtValue = expr, floorDivAssignStmtPos = pos} ->
      FloorDivAssignStmt name (transformExpr moduleAlias callAlias identAlias expr) pos
    PrintStmt {printStmtValue = expr, printStmtPos = pos} -> PrintStmt (transformExpr moduleAlias callAlias identAlias expr) pos
    ReturnStmt {returnStmtValue = expr, returnStmtPos = pos} -> ReturnStmt (transformExpr moduleAlias callAlias identAlias expr) pos
    YieldStmt {yieldStmtValue = expr, yieldStmtPos = pos} -> YieldStmt (transformExpr moduleAlias callAlias identAlias expr) pos
    YieldFromStmt {yieldFromStmtValue = expr, yieldFromStmtPos = pos} -> YieldFromStmt (transformExpr moduleAlias callAlias identAlias expr) pos
    IfStmt {ifStmtCond = cond, ifStmtThen = thenBranch, ifStmtElse = elseBranch, ifStmtPos = pos} ->
      IfStmt
        (transformExpr moduleAlias callAlias identAlias cond)
        (fmap (transformImportAliases renameDefNames moduleAlias callAlias identAlias) thenBranch)
        (fmap (fmap (transformImportAliases renameDefNames moduleAlias callAlias identAlias)) elseBranch)
        pos
    WhileStmt {whileStmtCond = cond, whileStmtBody = body, whileStmtPos = pos} ->
      WhileStmt (transformExpr moduleAlias callAlias identAlias cond) (fmap (transformImportAliases renameDefNames moduleAlias callAlias identAlias) body) pos
    ForStmt {forStmtVar = name, forStmtIter = expr, forStmtBody = body, forStmtPos = pos} ->
      ForStmt name (transformExpr moduleAlias callAlias identAlias expr) (fmap (transformImportAliases renameDefNames moduleAlias callAlias identAlias) body) pos
    FunctionDefStmt {functionDefStmtName = name, functionDefStmtParams = params, functionDefStmtBody = body, functionDefStmtPos = pos} ->
      FunctionDefStmt (renameName renameDefNames callAlias name) params (fmap (transformImportAliases renameDefNames moduleAlias callAlias identAlias) body) pos
    FunctionDefDefaultsStmt {functionDefDefaultsStmtName = name, functionDefDefaultsStmtParams = params, functionDefDefaultsStmtDefaults = defaults, functionDefDefaultsStmtBody = body, functionDefDefaultsStmtPos = pos} ->
      let renamedDefaults = fmap (\(paramName, defaultExpr) -> (paramName, transformExpr moduleAlias callAlias identAlias defaultExpr)) defaults
       in FunctionDefDefaultsStmt (renameName renameDefNames callAlias name) params renamedDefaults (fmap (transformImportAliases renameDefNames moduleAlias callAlias identAlias) body) pos
    _ -> stmt
  where
    renameName shouldRename renameMap name =
      if shouldRename
        then Map.findWithDefault name name renameMap
        else name

    transformExpr moduleAliases callAliases identAliases expr =
      case expr of
        IntegerExpr {} -> expr
        FloatExpr {} -> expr
        StringExpr {} -> expr
        NoneExpr {} -> expr
        IdentifierExpr {identifierExprName = name, identifierExprPos = pos} -> IdentifierExpr (Map.findWithDefault name name identAliases) pos
        ListExpr {listExprItems = items, listExprPos = pos} -> ListExpr (fmap (transformExpr moduleAliases callAliases identAliases) items) pos
        TupleExpr {tupleExprItems = items, tupleExprPos = pos} -> TupleExpr (fmap (transformExpr moduleAliases callAliases identAliases) items) pos
        ListComprehensionExpr {listComprehensionExprValue = valueExpr, listComprehensionExprLoopName = loopName, listComprehensionExprIter = iterExpr, listComprehensionExprPos = pos} ->
          ListComprehensionExpr (transformExpr moduleAliases callAliases identAliases valueExpr) loopName (transformExpr moduleAliases callAliases identAliases iterExpr) pos
        ListComprehensionClausesExpr {listComprehensionClausesExprValue = valueExpr, listComprehensionClausesExprClauses = clauses, listComprehensionClausesExprPos = pos} ->
          ListComprehensionClausesExpr
            (transformExpr moduleAliases callAliases identAliases valueExpr)
            (fmap (\(names, iterExpr, conds) -> (names, transformExpr moduleAliases callAliases identAliases iterExpr, fmap (transformExpr moduleAliases callAliases identAliases) conds)) clauses)
            pos
        DictExpr {dictExprEntries = entries, dictExprPos = pos} -> DictExpr (fmap (\(k, v) -> (transformExpr moduleAliases callAliases identAliases k, transformExpr moduleAliases callAliases identAliases v)) entries) pos
        KeywordArgExpr {keywordArgExprName = name, keywordArgExprValue = value, keywordArgExprPos = pos} -> KeywordArgExpr name (transformExpr moduleAliases callAliases identAliases value) pos
        StarArgExpr {starArgExprValue = value, starArgExprPos = pos} -> StarArgExpr (transformExpr moduleAliases callAliases identAliases value) pos
        KwStarArgExpr {kwStarArgExprValue = value, kwStarArgExprPos = pos} -> KwStarArgExpr (transformExpr moduleAliases callAliases identAliases value) pos
        WalrusExpr {walrusExprName = name, walrusExprValue = value, walrusExprPos = pos} -> WalrusExpr name (transformExpr moduleAliases callAliases identAliases value) pos
        LambdaExpr {lambdaExprParams = params, lambdaExprValue = valueExpr, lambdaExprPos = pos} -> LambdaExpr params (transformExpr moduleAliases callAliases identAliases valueExpr) pos
        LambdaDefaultsExpr {lambdaDefaultsExprParams = params, lambdaDefaultsExprDefaults = defaults, lambdaDefaultsExprValue = valueExpr, lambdaDefaultsExprPos = pos} ->
          LambdaDefaultsExpr params (fmap (\(paramName, defaultExpr) -> (paramName, transformExpr moduleAliases callAliases identAliases defaultExpr)) defaults) (transformExpr moduleAliases callAliases identAliases valueExpr) pos
        UnaryMinusExpr {unaryMinusExprValue = value, unaryMinusExprPos = pos} -> UnaryMinusExpr (transformExpr moduleAliases callAliases identAliases value) pos
        NotExpr {notExprValue = value, notExprPos = pos} -> NotExpr (transformExpr moduleAliases callAliases identAliases value) pos
        BinaryExpr {binaryExprOp = op, binaryExprLeft = left, binaryExprRight = right, binaryExprPos = pos} ->
          BinaryExpr op (transformExpr moduleAliases callAliases identAliases left) (transformExpr moduleAliases callAliases identAliases right) pos
        CallExpr {callExprName = fname, callExprArgs = args, callExprPos = pos} ->
          let renamedArgs = fmap (transformExpr moduleAliases callAliases identAliases) args
              renamedName = Map.findWithDefault fname fname callAliases
           in case renamedArgs of
                IdentifierExpr {identifierExprName = receiver} : restArgs ->
                  case Map.lookup receiver moduleAliases of
                    Just prefix -> CallExpr (prefix ++ renamedName) restArgs pos
                    Nothing -> CallExpr renamedName renamedArgs pos
                _ -> CallExpr renamedName renamedArgs pos
        CallValueExpr {callValueExprCallee = callee, callValueExprArgs = args, callValueExprPos = pos} ->
          CallValueExpr (transformExpr moduleAliases callAliases identAliases callee) (fmap (transformExpr moduleAliases callAliases identAliases) args) pos
        IndexExpr {indexExprBase = baseExpr, indexExprIndex = indexExpr, indexExprPos = pos} ->
          IndexExpr (transformExpr moduleAliases callAliases identAliases baseExpr) (transformExpr moduleAliases callAliases identAliases indexExpr) pos
        SliceExpr {sliceExprBase = baseExpr, sliceExprStart = maybeStart, sliceExprEnd = maybeEnd, sliceExprPos = pos} ->
          SliceExpr
            (transformExpr moduleAliases callAliases identAliases baseExpr)
            (fmap (transformExpr moduleAliases callAliases identAliases) maybeStart)
            (fmap (transformExpr moduleAliases callAliases identAliases) maybeEnd)
            pos
