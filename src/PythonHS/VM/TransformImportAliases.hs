module PythonHS.VM.TransformImportAliases (transformImportAliases) where

import qualified Data.Map.Strict as Map
import PythonHS.AST.Expr
  ( Expr
      ( BinaryExpr,
        CallExpr,
        DictExpr,
        FloatExpr,
        IdentifierExpr,
        IntegerExpr,
        KeywordArgExpr,
        ListExpr,
        NoneExpr,
        NotExpr,
        StringExpr,
        UnaryMinusExpr
      )
  )
import PythonHS.AST.Stmt
  ( Stmt
      ( AddAssignStmt,
        AssignStmt,
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
        WhileStmt
      )
  )

transformImportAliases :: Bool -> Map.Map String String -> Map.Map String String -> Map.Map String String -> Stmt -> Stmt
transformImportAliases renameDefNames moduleAlias callAlias identAlias stmt =
  case stmt of
    AssignStmt name expr pos -> AssignStmt (renameName renameDefNames callAlias name) (transformExpr moduleAlias callAlias identAlias expr) pos
    AddAssignStmt name expr pos -> AddAssignStmt name (transformExpr moduleAlias callAlias identAlias expr) pos
    SubAssignStmt name expr pos -> SubAssignStmt name (transformExpr moduleAlias callAlias identAlias expr) pos
    MulAssignStmt name expr pos -> MulAssignStmt name (transformExpr moduleAlias callAlias identAlias expr) pos
    DivAssignStmt name expr pos -> DivAssignStmt name (transformExpr moduleAlias callAlias identAlias expr) pos
    ModAssignStmt name expr pos -> ModAssignStmt name (transformExpr moduleAlias callAlias identAlias expr) pos
    FloorDivAssignStmt name expr pos -> FloorDivAssignStmt name (transformExpr moduleAlias callAlias identAlias expr) pos
    PrintStmt expr pos -> PrintStmt (transformExpr moduleAlias callAlias identAlias expr) pos
    ReturnStmt expr pos -> ReturnStmt (transformExpr moduleAlias callAlias identAlias expr) pos
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
        DictExpr entries pos -> DictExpr (fmap (\(k, v) -> (transformExpr moduleAliases callAliases identAliases k, transformExpr moduleAliases callAliases identAliases v)) entries) pos
        KeywordArgExpr name value pos -> KeywordArgExpr name (transformExpr moduleAliases callAliases identAliases value) pos
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
