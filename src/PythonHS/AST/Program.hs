module PythonHS.AST.Program (Program (..)) where

import PythonHS.AST.Stmt (Stmt)

newtype Program = Program {programStmts :: [Stmt]}
  deriving (Eq, Show)
