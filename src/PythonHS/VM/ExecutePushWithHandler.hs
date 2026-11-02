module PythonHS.VM.ExecutePushWithHandler (executePushWithHandler) where

import PythonHS.VM.ExceptionState (ExceptionState (..))
import PythonHS.VM.ExecutePushWithHandlerConfig (ExecutePushWithHandlerConfig (..))
import PythonHS.VM.VMState (VMState (..))

executePushWithHandler :: ExecutePushWithHandlerConfig -> Either String VMState
executePushWithHandler config = do
  let state = executePushWithHandlerState config
      handlerIp = executePushWithHandlerIp config
  if handlerIp < 0 || handlerIp >= length (vmCode state)
    then Left ("Runtime error: invalid handler IP " ++ show handlerIp ++ " for PushWithHandler")
    else
      executePushWithHandlerExecute
        config
        state {vmIp = vmIp state + 1, vmException = (vmException state) {exceptionHandlers = handlerIp : exceptionHandlers (vmException state)}}
