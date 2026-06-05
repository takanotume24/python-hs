module PythonHS.AST.WithContext (ContextManager(..), WithEntry(..), WithExit(..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.Instruction (Instruction)

-- | Record to encapsulate context manager data
data ContextManager = ContextManager
  { contextManagerExpr :: Expr
  , contextManagerVarName :: Maybe String
  , contextManagerPos :: Position
  } deriving (Eq, Show)

-- | Record to encapsulate context manager entry (__enter__) call data
data WithEntry = WithEntry
  { entryCallExpr :: Expr
  , entryCallInstruction :: Instruction
  , entryPos :: Position
  } deriving (Eq, Show)

-- | Record to encapsulate context manager exit (__exit__) call data
data WithExit = WithExit
  { exitCallExpr :: Expr
  , exitCallInstruction :: Instruction
  , exitPos :: Position
  , exitIsException :: Bool
  } deriving (Eq, Show)