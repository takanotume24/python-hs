module PythonHS.AST.Program (Program (..)) where

import PythonHS.AST.Stmt (Stmt)

newtype Program = Program [Stmt]
  deriving (Eq, Show)
