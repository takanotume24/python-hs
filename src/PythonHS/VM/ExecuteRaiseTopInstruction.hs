module PythonHS.VM.ExecuteRaiseTopInstruction (executeRaiseTopInstruction) where

import Data.Map.Strict qualified as Map
import PythonHS.Evaluator.Value (Value (IntValue, StringValue))
import PythonHS.VM.DecodeFinallyHandler (decodeFinallyHandler)
import PythonHS.VM.EnvState (EnvState (..))
import PythonHS.VM.ExceptionState (ExceptionState (..))
import PythonHS.VM.ExecuteRaiseTopInstructionConfig (ExecuteRaiseTopInstructionConfig (..))
import PythonHS.VM.IsFinallyHandler (isFinallyHandler)
import PythonHS.VM.LoopBoundaryMarker (loopBoundaryMarker)
import PythonHS.VM.PendingFinallyErrorName (pendingFinallyErrorName)
import PythonHS.VM.PendingFinallyOwnerName (pendingFinallyOwnerName)
import PythonHS.VM.RaiseTopError (raiseTopError)
import PythonHS.VM.RaiseTopErrorConfig (RaiseTopErrorConfig (..))
import PythonHS.VM.VMState (VMState (..))

executeRaiseTopInstruction :: ExecuteRaiseTopInstructionConfig -> Either String VMState
executeRaiseTopInstruction config =
  case vmStack state of
    value : rest ->
      let err = raiseTopError RaiseTopErrorConfig {raiseTopErrorValue = value, raiseTopErrorPos = pos}
          newLocals =
            Map.insert pendingExceptErrorName (StringValue err) $
              Map.insert pendingExceptValueName value $
                envLocals (vmEnv state)
          newEnv = (vmEnv state) {envLocals = newLocals}
       in dispatch err rest newEnv newLocals (exceptionHandlers (vmException state))
    _ -> Left "VM runtime error: raise requires one value on stack"
  where
    state = executeRaiseTopInstructionState config
    pos = executeRaiseTopInstructionPos config
    exceptionState = vmException state

    dispatch err rest newEnv newLocals handlers =
      case handlers of
        [] -> Left err
        handlerIp : restHandlers
          | handlerIp == loopBoundaryMarker -> dispatch err rest newEnv newLocals restHandlers
          | isFinallyHandler handlerIp ->
              let finallyLocals =
                    Map.insert pendingFinallyOwnerName (IntValue (fromIntegral (decodeFinallyHandler handlerIp))) $
                      Map.insert pendingFinallyErrorName (StringValue err) newLocals
               in Right state {vmIp = decodeFinallyHandler handlerIp + 1, vmStack = rest, vmEnv = newEnv {envLocals = finallyLocals}, vmException = exceptionState {exceptionHandlers = restHandlers}}
          | otherwise ->
              let handledLocals = Map.delete pendingFinallyOwnerName (Map.delete pendingFinallyErrorName newLocals)
               in Right state {vmIp = handlerIp, vmStack = rest, vmEnv = newEnv {envLocals = handledLocals}, vmException = exceptionState {exceptionHandlers = restHandlers}}

    pendingExceptErrorName = "__python_hs_pending_except_error__"
    pendingExceptValueName = "__python_hs_pending_except_value__"
