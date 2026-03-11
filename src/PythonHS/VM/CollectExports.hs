module PythonHS.VM.CollectExports (collectExports) where

import qualified Data.Map.Strict as Map
import PythonHS.AST.Stmt
  ( Stmt
      ( AnnAssignStmt,
        AssignStmt,
        AssignUnpackStmt,
        DecoratedStmt,
        FunctionDefDefaultsStmt,
        FunctionDefStmt
      )
  )
import PythonHS.VM.ModulePrefixFor (modulePrefixFor)

collectExports :: [String] -> [Stmt] -> Map.Map String String
collectExports modulePath stmts =
  foldl
    ( \acc stmt ->
        case stmt of
          AssignStmt name _ _ -> Map.insert name (moduleMemberName name) acc
          AssignUnpackStmt names _ _ -> foldl (\m name -> Map.insert name (moduleMemberName name) m) acc names
          AnnAssignStmt name _ (Just _) _ -> Map.insert name (moduleMemberName name) acc
          FunctionDefStmt name _ _ _ -> Map.insert name (moduleMemberName name) acc
          FunctionDefDefaultsStmt name _ _ _ _ -> Map.insert name (moduleMemberName name) acc
          DecoratedStmt _ innerStmt _ ->
            case innerStmt of
              FunctionDefStmt name _ _ _ -> Map.insert name (moduleMemberName name) acc
              FunctionDefDefaultsStmt name _ _ _ _ -> Map.insert name (moduleMemberName name) acc
              _ -> acc
          _ -> acc
    )
    Map.empty
    stmts
  where
    moduleMemberName name = modulePrefixFor modulePath ++ name
