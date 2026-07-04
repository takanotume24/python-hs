module PythonHS.VM.HandleRuntimeError (handleRuntimeError) where

import Data.Map.Strict qualified as Map
import PythonHS.Evaluator.Value (Value (StringValue))
import PythonHS.VM.EnvState (EnvState (..))
import PythonHS.VM.ExceptionState (ExceptionState (..))
import PythonHS.VM.VMState (VMState (..))

handleRuntimeError ::
  (VMState -> Either String VMState) ->
  VMState ->
  Either String VMState ->
  Either String VMState
handleRuntimeError execute state result =
  case result of
    Right value -> Right value
    Left err ->
      case exceptionHandlers (vmException state) of
        handlerIp : restHandlers ->
          if isFinallyHandler handlerIp
            then
              let finalIp = decodeFinallyHandler handlerIp
                  newLocals = Map.insert pendingErrorName (StringValue err) (envLocals (vmEnv state))
                  newEnv = (vmEnv state) {envLocals = newLocals}
               in execute state {vmIp = finalIp, vmEnv = newEnv, vmException = (vmException state) {exceptionHandlers = restHandlers}}
            else
              let newLocals = Map.insert pendingExceptErrorName (StringValue err) (Map.delete pendingErrorName (envLocals (vmEnv state)))
                  newEnv = (vmEnv state) {envLocals = newLocals}
               in execute state {vmIp = handlerIp, vmEnv = newEnv, vmException = (vmException state) {exceptionHandlers = restHandlers}}
        [] -> Left err
  where
    pendingErrorName = "__python_hs_pending_finally_error__"
    pendingExceptErrorName = "__python_hs_pending_except_error__"
    decodeFinallyHandler encoded = negate encoded - 1
    isFinallyHandler handlerIp = handlerIp < 0
