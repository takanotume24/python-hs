module PythonHS.VM.ResolveStarExportNames (resolveStarExportNames) where

import Data.List (isPrefixOf)
import Data.Map.Strict qualified as Map
import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.Stmt (Stmt (..))

resolveStarExportNames :: [Stmt] -> Map.Map String String -> [(String, String)]
resolveStarExportNames moduleStmts exportMap =
  case extractAllNames moduleStmts exportMap of
    Just allNames -> filter (\(name, _) -> name `elem` allNames) (Map.toList exportMap)
    Nothing -> Map.toList (Map.filterWithKey (\name _ -> not ("_" `isPrefixOf` name)) exportMap)
  where
    extractAllNames stmts exports =
      case Map.lookup "__all__" exports of
        Nothing -> Nothing
        Just mappedAllName ->
          case findAssignedExpr mappedAllName stmts of
            Nothing -> Nothing
            Just assignedExpr -> collectStringNames assignedExpr

    findAssignedExpr targetName stmts =
      case stmts of
        [] -> Nothing
        stmt : rest ->
          case stmt of
            AssignStmt {assignStmtName = name, assignStmtValue = expr}
              | name == targetName -> Just expr
            _ -> findAssignedExpr targetName rest

    collectStringNames expr =
      case expr of
        ListExpr {listExprItems = items} -> collectItems items
        TupleExpr {tupleExprItems = items} -> collectItems items
        _ -> Nothing
      where
        collectItems items =
          case items of
            [] -> Just []
            item : rest ->
              case (item, collectItems rest) of
                (StringExpr {stringExprValue = name}, Just names) -> Just (name : names)
                _ -> Nothing
