module PythonHS.VM.ResolveStarExportNames (resolveStarExportNames) where

import qualified Data.Map.Strict as Map
import Data.List (isPrefixOf)
import PythonHS.AST.Expr (Expr (ListExpr, StringExpr, TupleExpr))
import PythonHS.AST.Stmt (Stmt (AssignStmt))

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
            AssignStmt name expr _
              | name == targetName -> Just expr
            _ -> findAssignedExpr targetName rest

    collectStringNames expr =
      case expr of
        ListExpr items _ -> collectItems items
        TupleExpr items _ -> collectItems items
        _ -> Nothing
      where
        collectItems items =
          case items of
            [] -> Just []
            item : rest ->
              case (item, collectItems rest) of
                (StringExpr name _, Just names) -> Just (name : names)
                _ -> Nothing
