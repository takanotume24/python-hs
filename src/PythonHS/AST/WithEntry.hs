module PythonHS.AST.WithEntry (WithEntry(..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.Instruction (Instruction)

-- | Record to encapsulate context manager entry (__enter__) call data
data WithEntry = WithEntry
  { entryCallExpr :: Expr
  , entryCallInstruction :: Instruction
  , entryPos :: Position
  } deriving (Eq, Show)
