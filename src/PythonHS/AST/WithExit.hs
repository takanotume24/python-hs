module PythonHS.AST.WithExit (WithExit (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.Instruction (Instruction)

-- | Record to encapsulate context manager exit (__exit__) call data
data WithExit = WithExit
  { exitCallExpr :: Expr,
    exitCallInstruction :: Instruction,
    exitPos :: Position,
    exitIsException :: Bool
  }
  deriving (Eq, Show)
