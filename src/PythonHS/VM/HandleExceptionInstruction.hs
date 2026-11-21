module PythonHS.VM.HandleExceptionInstruction (handleExceptionInstruction) where

import Data.Map.Strict qualified as Map
import PythonHS.VM.ApplyExceptionInstruction (applyExceptionInstruction)
import PythonHS.VM.EnvState (EnvState (..))
import PythonHS.VM.ExceptionApplyResult (ExceptionApplyResult (..))
import PythonHS.VM.ExceptionState (ExceptionState (..))
import PythonHS.VM.HandleExceptionInstructionConfig (HandleExceptionInstructionConfig (..))
import PythonHS.VM.Instruction (Instruction (..))
import PythonHS.VM.VMState (VMState (..))

handleExceptionInstruction :: HandleExceptionInstructionConfig -> Either String VMState
handleExceptionInstruction config = case instruction of
  PopExceptionHandler ->
    if null (exceptionHandlers (vmException state))
      then Left "Runtime error: attempting to pop from empty exception handler stack"
      else processExceptionInstruction
  _ -> processExceptionInstruction
  where
    state = handleExceptionInstructionState config
    instruction = handleExceptionInstructionInstruction config
    processExceptionInstruction =
      case applyExceptionInstruction (vmIp state) instruction (vmStack state) (Map.union (envLocals (vmEnv state)) (envGlobals (vmEnv state))) (exceptionHandlers (vmException state)) of
        Left err -> Left err
        Right applyResult ->
          case exceptionApplyResultError applyResult of
            Just err -> Left err
            Nothing ->
              let newLocals = foldl applyLocalChange (envLocals (vmEnv state)) (exceptionApplyResultLocalChanges applyResult)
               in Right
                    state
                      { vmIp = exceptionApplyResultIp applyResult,
                        vmStack = exceptionApplyResultStack applyResult,
                        vmEnv = (vmEnv state) {envLocals = newLocals},
                        vmException = (vmException state) {exceptionHandlers = exceptionApplyResultHandlers applyResult}
                      }
    applyLocalChange locals (name, maybeValue) =
      case maybeValue of
        Nothing -> Map.delete name locals
        Just value -> Map.insert name value locals
