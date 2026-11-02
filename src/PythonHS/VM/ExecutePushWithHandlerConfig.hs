module PythonHS.VM.ExecutePushWithHandlerConfig (ExecutePushWithHandlerConfig (..)) where

import PythonHS.VM.VMState (VMState)

data ExecutePushWithHandlerConfig = ExecutePushWithHandlerConfig
  { executePushWithHandlerExecute :: VMState -> Either String VMState,
    executePushWithHandlerState :: VMState,
    executePushWithHandlerIp :: Int
  }
