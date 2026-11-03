module PythonHS.VM.ExecuteCheckWithResultConfig (ExecuteCheckWithResultConfig (..)) where

import PythonHS.VM.VMState (VMState)

data ExecuteCheckWithResultConfig = ExecuteCheckWithResultConfig
  { executeCheckWithResultState :: VMState
  }
