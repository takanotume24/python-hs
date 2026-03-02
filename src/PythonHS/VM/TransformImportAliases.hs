module PythonHS.VM.TransformImportAliases (transformImportAliases) where

import qualified Data.Map.Strict as Map
import PythonHS.AST.Expr
  ( Expr
      ( BinaryExpr,
        CallExpr,
        CallValueExpr,
        DictExpr,
        FloatExpr,
        IdentifierExpr,
        IntegerExpr,
        KeywordArgExpr,
        KwStarArgExpr,
        LambdaDefaultsExpr,
        LambdaExpr,
        ListComprehensionClausesExpr,
        ListComprehensionExpr,
         ListExpr,
         NoneExpr,
         NotExpr,
         StarArgExpr,
         StringExpr,
         UnaryMinusExpr,
         WalrusExpr
      )
  )
import PythonHS.AST.Stmt
  ( Stmt
      ( AddAssignStmt,
        AssignStmt,
        DecoratedStmt,
        DivAssignStmt,
        FloorDivAssignStmt,
        ForStmt,
        FunctionDefDefaultsStmt,
        FunctionDefStmt,
        IfStmt,
        ModAssignStmt,
        MulAssignStmt,
        PrintStmt,
        ReturnStmt,
        SubAssignStmt,
        WhileStmt,
        YieldFromStmt,
        YieldStmt
      )
  )

transformImportAliases :: Bool -> Map.Map String String -> Map.Map String String -> Map.Map String String -> Stmt -> Stmt
transformImportAliases renameDefNames moduleAlias callAlias identAlias stmt =
  case stmt of
    AssignStmt name expr pos -> AssignStmt (renameName renameDefNames callAlias name) (transformExpr moduleAlias callAlias identAlias expr) pos
    DecoratedStmt decorators innerStmt pos ->
      DecoratedStmt
        (fmap (transformExpr moduleAlias callAlias identAlias) decorators)
        (transformImportAliases renameDefNames moduleAlias callAlias identAlias innerStmt)
        pos
    AddAssignStmt name expr pos -> AddAssignStmt name (transformExpr moduleAlias callAlias identAlias expr) pos
    SubAssignStmt name expr pos -> SubAssignStmt name (transformExpr moduleAlias callAlias identAlias expr) pos
    MulAssignStmt name expr pos -> MulAssignStmt name (transformExpr moduleAlias callAlias identAlias expr) pos
    DivAssignStmt name expr pos -> DivAssignStmt name (transformExpr moduleAlias callAlias identAlias expr) pos
    ModAssignStmt name expr pos -> ModAssignStmt name (transformExpr moduleAlias callAlias identAlias expr) pos
    FloorDivAssignStmt name expr pos -> FloorDivAssignStmt name (transformExpr moduleAlias callAlias identAlias expr) pos
    PrintStmt expr pos -> PrintStmt (transformExpr moduleAlias callAlias identAlias expr) pos
    ReturnStmt expr pos -> ReturnStmt (transformExpr moduleAlias callAlias identAlias expr) pos
    YieldStmt expr pos -> YieldStmt (transformExpr moduleAlias callAlias identAlias expr) pos
    YieldFromStmt expr pos -> YieldFromStmt (transformExpr moduleAlias callAlias identAlias expr) pos
    IfStmt cond thenBranch elseBranch pos ->
      IfStmt
        (transformExpr moduleAlias callAlias identAlias cond)
        (fmap (transformImportAliases renameDefNames moduleAlias callAlias identAlias) thenBranch)
        (fmap (fmap (transformImportAliases renameDefNames moduleAlias callAlias identAlias)) elseBranch)
        pos
    WhileStmt cond body pos -> WhileStmt (transformExpr moduleAlias callAlias identAlias cond) (fmap (transformImportAliases renameDefNames moduleAlias callAlias identAlias) body) pos
    ForStmt name expr body pos -> ForStmt name (transformExpr moduleAlias callAlias identAlias expr) (fmap (transformImportAliases renameDefNames moduleAlias callAlias identAlias) body) pos
    FunctionDefStmt name params body pos ->
      FunctionDefStmt (renameName renameDefNames callAlias name) params (fmap (transformImportAliases renameDefNames moduleAlias callAlias identAlias) body) pos
    FunctionDefDefaultsStmt name params defaults body pos ->
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
        IntegerExpr _ _ -> expr
        FloatExpr _ _ -> expr
        StringExpr _ _ -> expr
        NoneExpr _ -> expr
        IdentifierExpr name pos -> IdentifierExpr (Map.findWithDefault name name identAliases) pos
        ListExpr items pos -> ListExpr (fmap (transformExpr moduleAliases callAliases identAliases) items) pos
        ListComprehensionExpr valueExpr loopName iterExpr pos ->
          ListComprehensionExpr (transformExpr moduleAliases callAliases identAliases valueExpr) loopName (transformExpr moduleAliases callAliases identAliases iterExpr) pos
        ListComprehensionClausesExpr valueExpr clauses pos ->
          ListComprehensionClausesExpr
            (transformExpr moduleAliases callAliases identAliases valueExpr)
            (fmap (\(names, iterExpr, conds) -> (names, transformExpr moduleAliases callAliases identAliases iterExpr, fmap (transformExpr moduleAliases callAliases identAliases) conds)) clauses)
            pos
        DictExpr entries pos -> DictExpr (fmap (\(k, v) -> (transformExpr moduleAliases callAliases identAliases k, transformExpr moduleAliases callAliases identAliases v)) entries) pos
        KeywordArgExpr name value pos -> KeywordArgExpr name (transformExpr moduleAliases callAliases identAliases value) pos
        StarArgExpr value pos -> StarArgExpr (transformExpr moduleAliases callAliases identAliases value) pos
        KwStarArgExpr value pos -> KwStarArgExpr (transformExpr moduleAliases callAliases identAliases value) pos
        WalrusExpr name value pos -> WalrusExpr name (transformExpr moduleAliases callAliases identAliases value) pos
        LambdaExpr params valueExpr pos -> LambdaExpr params (transformExpr moduleAliases callAliases identAliases valueExpr) pos
        LambdaDefaultsExpr params defaults valueExpr pos ->
          LambdaDefaultsExpr params (fmap (\(paramName, defaultExpr) -> (paramName, transformExpr moduleAliases callAliases identAliases defaultExpr)) defaults) (transformExpr moduleAliases callAliases identAliases valueExpr) pos
        UnaryMinusExpr value pos -> UnaryMinusExpr (transformExpr moduleAliases callAliases identAliases value) pos
        NotExpr value pos -> NotExpr (transformExpr moduleAliases callAliases identAliases value) pos
        BinaryExpr op left right pos -> BinaryExpr op (transformExpr moduleAliases callAliases identAliases left) (transformExpr moduleAliases callAliases identAliases right) pos
        CallExpr fname args pos ->
          let renamedArgs = fmap (transformExpr moduleAliases callAliases identAliases) args
              renamedName = Map.findWithDefault fname fname callAliases
           in case renamedArgs of
                IdentifierExpr receiver _ : restArgs ->
                  case Map.lookup receiver moduleAliases of
                    Just prefix -> CallExpr (prefix ++ renamedName) restArgs pos
                    Nothing -> CallExpr renamedName renamedArgs pos
                _ -> CallExpr renamedName renamedArgs pos
        CallValueExpr callee args pos ->
          CallValueExpr (transformExpr moduleAliases callAliases identAliases callee) (fmap (transformExpr moduleAliases callAliases identAliases) args) pos
