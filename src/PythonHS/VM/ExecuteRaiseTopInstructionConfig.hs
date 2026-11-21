module PythonHS.VM.ExecuteRaiseTopInstructionConfig (ExecuteRaiseTopInstructionConfig (..)) where

import PythonHS.Lexer.Position (Position)
import PythonHS.VM.VMState (VMState)

data ExecuteRaiseTopInstructionConfig = ExecuteRaiseTopInstructionConfig
  { executeRaiseTopInstructionState :: VMState,
    executeRaiseTopInstructionPos :: Position
  }
