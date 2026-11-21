module PythonHS.VM.ExecuteControlFlowInstruction (executeControlFlowInstruction) where

import Data.Map.Strict qualified as Map
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.DropToLoopBoundary (dropToLoopBoundary)
import PythonHS.VM.EnvState (EnvState (..))
import PythonHS.VM.ExceptionState (ExceptionState (..))
import PythonHS.VM.ExecuteControlFlowInstructionConfig (ExecuteControlFlowInstructionConfig (..))
import PythonHS.VM.Instruction (Instruction (..))
import PythonHS.VM.LoopBoundaryMarker (loopBoundaryMarker)
import PythonHS.VM.PendingControlKindName (pendingControlKindName)
import PythonHS.VM.PendingControlOwnerName (pendingControlOwnerName)
import PythonHS.VM.PendingControlTargetName (pendingControlTargetName)
import PythonHS.VM.PendingFinallyErrorName (pendingFinallyErrorName)
import PythonHS.VM.PendingFinallyOwnerName (pendingFinallyOwnerName)
import PythonHS.VM.PendingReturnValueName (pendingReturnValueName)
import PythonHS.VM.TakeFinallyHandler (takeFinallyHandler)
import PythonHS.VM.VMState (VMState (..))

executeControlFlowInstruction :: ExecuteControlFlowInstructionConfig -> Maybe (Either String VMState)
executeControlFlowInstruction config =
  case instruction of
    PushLoopBoundary -> Just (Right state {vmIp = ip + 1, vmException = exceptionState {exceptionHandlers = loopBoundaryMarker : handlers}})
    PopLoopBoundary -> Just (Right state {vmIp = ip + 1, vmException = exceptionState {exceptionHandlers = dropToLoopBoundary True handlers}})
    BreakLoop {breakLoopTarget = target} -> Just (exitLoop "break" True target)
    ContinueLoop {continueLoopTarget = target} -> Just (exitLoop "continue" False target)
    ReturnTop -> Just returnFromFunction
    _ -> Nothing
  where
    state = executeControlFlowInstructionState config
    instruction = executeControlFlowInstructionInstruction config
    ip = vmIp state
    handlers = exceptionHandlers (vmException state)
    exceptionState = vmException state
    locals = envLocals (vmEnv state)

    returnFromFunction =
      case vmStack state of
        value : restStack ->
          case takeFinallyHandler False handlers of
            Just (restHandlers, nextFinallyStart) ->
              let newLocals =
                    Map.insert pendingControlKindName (StringValue "return") $
                      Map.insert pendingControlOwnerName (IntValue (fromIntegral nextFinallyStart)) $
                        Map.insert pendingReturnValueName value $
                          Map.delete pendingFinallyErrorName $
                            Map.delete pendingFinallyOwnerName locals
               in Right state {vmIp = nextFinallyStart + 1, vmStack = restStack, vmEnv = (vmEnv state) {envLocals = newLocals}, vmException = exceptionState {exceptionHandlers = restHandlers}}
            Nothing -> Right state {vmStack = [value]}
        _ -> Left "VM runtime error: return requires one value on stack"

    exitLoop controlKind includeBoundary target =
      case takeFinallyHandler True handlers of
        Just (restHandlers, nextFinallyStart) ->
          let newLocals =
                Map.insert pendingControlKindName (StringValue controlKind) $
                  Map.insert pendingControlOwnerName (IntValue (fromIntegral nextFinallyStart)) $
                    Map.insert pendingControlTargetName (IntValue (fromIntegral target)) $
                      Map.delete pendingReturnValueName $
                        Map.delete pendingFinallyErrorName $
                          Map.delete pendingFinallyOwnerName locals
           in Right state {vmIp = nextFinallyStart + 1, vmEnv = (vmEnv state) {envLocals = newLocals}, vmException = exceptionState {exceptionHandlers = restHandlers}}
        Nothing ->
          Right
            state
              { vmIp = target,
                vmEnv = (vmEnv state) {envLocals = clearControl locals},
                vmException = exceptionState {exceptionHandlers = dropToLoopBoundary includeBoundary handlers}
              }

    clearControl =
      Map.delete pendingControlKindName
        . Map.delete pendingControlTargetName
        . Map.delete pendingControlOwnerName
        . Map.delete pendingReturnValueName
        . Map.delete pendingFinallyErrorName
        . Map.delete pendingFinallyOwnerName
