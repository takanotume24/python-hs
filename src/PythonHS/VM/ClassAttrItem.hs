module PythonHS.VM.ClassAttrItem (ClassAttrItem (..)) where

import PythonHS.AST.Expr (Expr)

data ClassAttrItem = ClassAttrItem
  { classAttrItemName :: String,
    classAttrItemExpr :: Expr
  }
