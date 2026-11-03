module PythonHS.VM.ExecutePushWithHandlerConfig (ExecutePushWithHandlerConfig (..)) where

import PythonHS.VM.VMState (VMState)

data ExecutePushWithHandlerConfig = ExecutePushWithHandlerConfig
  { executePushWithHandlerState :: VMState,
    executePushWithHandlerIp :: Int
  }
