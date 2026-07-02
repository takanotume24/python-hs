module PythonHS.VM.HandleExceptionInstruction (handleExceptionInstruction) where

import PythonHS.VM.ApplyExceptionInstruction (applyExceptionInstruction)
import PythonHS.VM.EnvState (EnvState (..))
import PythonHS.VM.ExceptionState (ExceptionState (..))
import PythonHS.VM.Instruction (Instruction (..))
import PythonHS.VM.VMState (VMState (..))

handleExceptionInstruction ::
  (VMState -> Either String VMState) ->
  VMState ->
  Instruction ->
  Either String VMState
handleExceptionInstruction execute state instruction =
  case instruction of
    PopExceptionHandler ->
      if null (exceptionHandlers (vmException state))
        then Left "Runtime error: attempting to pop from empty exception handler stack"
        else processExceptionInstruction
    _ -> processExceptionInstruction
  where
    processExceptionInstruction =
      case applyExceptionInstruction (vmIp state) instruction (vmStack state) (envLocals (vmEnv state)) (exceptionHandlers (vmException state)) of
        Left err -> Left err
        Right (Just err, _, _, _) -> Left err
        Right (Nothing, nextIp, nextStack, nextHandlers) ->
          execute state {vmIp = nextIp, vmStack = nextStack, vmException = (vmException state) {exceptionHandlers = nextHandlers}}
