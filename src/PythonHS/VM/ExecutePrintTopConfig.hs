module PythonHS.VM.ExecutePrintTopConfig (ExecutePrintTopConfig (..)) where

import PythonHS.VM.VMState (VMState)

data ExecutePrintTopConfig = ExecutePrintTopConfig
  { executePrintTopExecute :: VMState -> Either String VMState,
    executePrintTopState :: VMState
  }
