module PythonHS.VM.ExecuteJumpIfFalseConfig (ExecuteJumpIfFalseConfig (..)) where

import PythonHS.VM.VMState (VMState)

data ExecuteJumpIfFalseConfig = ExecuteJumpIfFalseConfig
  { executeJumpIfFalseExecute :: VMState -> Either String VMState,
    executeJumpIfFalseState :: VMState,
    executeJumpIfFalseTarget :: Int
  }
