module PythonHS.VM.ExecuteLoopGuardConfig (ExecuteLoopGuardConfig (..)) where

import PythonHS.Lexer.Position (Position)
import PythonHS.VM.VMState (VMState)

data ExecuteLoopGuardConfig = ExecuteLoopGuardConfig
  { executeLoopGuardState :: VMState,
    executeLoopGuardPos :: Position
  }
