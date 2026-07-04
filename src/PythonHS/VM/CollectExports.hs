module PythonHS.VM.CollectExports (collectExports) where

import Data.Map.Strict qualified as Map
import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.VM.ModulePrefixFor (modulePrefixFor)

collectExports :: [String] -> [Stmt] -> Map.Map String String
collectExports modulePath stmts =
  foldl
    ( \acc stmt ->
        case stmt of
          AssignStmt {assignStmtName = name} -> Map.insert name (moduleMemberName name) acc
          AssignUnpackStmt {assignUnpackStmtNames = names} -> foldl (\m name -> Map.insert name (moduleMemberName name) m) acc names
          AnnAssignStmt {annAssignStmtName = name, annAssignStmtValue = Just _} -> Map.insert name (moduleMemberName name) acc
          FunctionDefStmt {functionDefStmtName = name} -> Map.insert name (moduleMemberName name) acc
          FunctionDefDefaultsStmt {functionDefDefaultsStmtName = name} -> Map.insert name (moduleMemberName name) acc
          DecoratedStmt {decoratedStmtTarget = innerStmt} ->
            case innerStmt of
              FunctionDefStmt {functionDefStmtName = name} -> Map.insert name (moduleMemberName name) acc
              FunctionDefDefaultsStmt {functionDefDefaultsStmtName = name} -> Map.insert name (moduleMemberName name) acc
              _ -> acc
          _ -> acc
    )
    Map.empty
    stmts
  where
    moduleMemberName name = modulePrefixFor modulePath ++ name
