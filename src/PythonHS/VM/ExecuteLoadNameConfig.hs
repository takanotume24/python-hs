module PythonHS.VM.ExecuteLoadNameConfig (ExecuteLoadNameConfig (..)) where

import PythonHS.Lexer.Position (Position)
import PythonHS.VM.VMState (VMState)

data ExecuteLoadNameConfig = ExecuteLoadNameConfig
  { executeLoadNameState :: VMState,
    executeLoadNameName :: String,
    executeLoadNamePos :: Position
  }
