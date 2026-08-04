module PythonHS.VM.FieldInfo (FieldInfo (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Position (Position)

data FieldInfo = FieldInfo
  { fieldInfoName :: String,
    fieldInfoDefault :: Maybe Expr,
    fieldInfoPos :: Position
  }
  deriving (Eq, Show)
