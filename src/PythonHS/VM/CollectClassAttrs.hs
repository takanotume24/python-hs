module PythonHS.VM.CollectClassAttrs (collectClassAttrs) where

import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.VM.ClassAttrItem (ClassAttrItem (..))

collectClassAttrs :: [Stmt] -> [ClassAttrItem]
collectClassAttrs items =
  case items of
    [] -> []
    item : restItems ->
      case item of
        AssignStmt {assignStmtName = name, assignStmtValue = value} ->
          if isSimpleName name
            then ClassAttrItem {classAttrItemName = name, classAttrItemExpr = value} : collectClassAttrs restItems
            else collectClassAttrs restItems
        _ -> collectClassAttrs restItems
  where
    isSimpleName name = not (elem '.' name)
