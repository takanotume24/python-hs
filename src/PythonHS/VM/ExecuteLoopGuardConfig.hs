module PythonHS.VM.ExecuteLoopGuardConfig (ExecuteLoopGuardConfig (..)) where

import PythonHS.Lexer.Position (Position)
import PythonHS.VM.VMState (VMState)

data ExecuteLoopGuardConfig = ExecuteLoopGuardConfig
  { executeLoopGuardExecute :: VMState -> Either String VMState,
    executeLoopGuardState :: VMState,
    executeLoopGuardPos :: Position
  }
