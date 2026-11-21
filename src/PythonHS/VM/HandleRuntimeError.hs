module PythonHS.VM.HandleRuntimeError (handleRuntimeError) where

import Data.Map.Strict qualified as Map
import PythonHS.Evaluator.Value (Value (IntValue, StringValue))
import PythonHS.VM.DecodeFinallyHandler (decodeFinallyHandler)
import PythonHS.VM.EnvState (EnvState (..))
import PythonHS.VM.ExceptionState (ExceptionState (..))
import PythonHS.VM.IsFinallyHandler (isFinallyHandler)
import PythonHS.VM.LoopBoundaryMarker (loopBoundaryMarker)
import PythonHS.VM.PendingFinallyErrorName (pendingFinallyErrorName)
import PythonHS.VM.PendingFinallyOwnerName (pendingFinallyOwnerName)
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
        handlerIp : restHandlers
          | handlerIp == loopBoundaryMarker ->
              handleRuntimeError execute state {vmException = (vmException state) {exceptionHandlers = restHandlers}} (Left err)
          | isFinallyHandler handlerIp ->
              let finalIp = decodeFinallyHandler handlerIp + 1
                  newLocals =
                    Map.insert pendingFinallyOwnerName (IntValue (fromIntegral (decodeFinallyHandler handlerIp))) $
                      Map.insert pendingFinallyErrorName (StringValue err) (envLocals (vmEnv state))
                  newEnv = (vmEnv state) {envLocals = newLocals}
               in execute state {vmIp = finalIp, vmEnv = newEnv, vmException = (vmException state) {exceptionHandlers = restHandlers}}
          | otherwise ->
              let newLocals =
                    Map.insert pendingExceptErrorName (StringValue err) $
                      Map.insert pendingExceptValueName (StringValue err) $
                        Map.delete pendingFinallyOwnerName $
                          Map.delete pendingFinallyErrorName (envLocals (vmEnv state))
                  newEnv = (vmEnv state) {envLocals = newLocals}
               in execute state {vmIp = handlerIp, vmEnv = newEnv, vmException = (vmException state) {exceptionHandlers = restHandlers}}
        [] -> Left err
  where
    pendingExceptErrorName = "__python_hs_pending_except_error__"
    pendingExceptValueName = "__python_hs_pending_except_value__"
