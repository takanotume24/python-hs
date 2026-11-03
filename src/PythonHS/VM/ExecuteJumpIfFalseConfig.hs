module PythonHS.VM.ExecuteJumpIfFalseConfig (ExecuteJumpIfFalseConfig (..)) where

import PythonHS.VM.VMState (VMState)

data ExecuteJumpIfFalseConfig = ExecuteJumpIfFalseConfig
  { executeJumpIfFalseState :: VMState,
    executeJumpIfFalseTarget :: Int
  }
