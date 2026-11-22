module PythonHS.VM.CollectClassAttrs (collectClassAttrs) where

import PythonHS.AST.Expr (Expr (..))
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
        AnnAssignStmt {annAssignStmtName = name, annAssignStmtValue = Just value} ->
          if isSimpleName name && not (isDataclassFieldCall value)
            then ClassAttrItem {classAttrItemName = name, classAttrItemExpr = value} : collectClassAttrs restItems
            else collectClassAttrs restItems
        _ -> collectClassAttrs restItems
  where
    isSimpleName name = not (elem '.' name)
    isDataclassFieldCall CallExpr {callExprName = "field"} = True
    isDataclassFieldCall _ = False
