module PythonHS.VM.RunInstructionsTrace (runInstructionsTrace) where

import PythonHS.VM.ExceptionState (ExceptionState (..))
import PythonHS.VM.ExecuteOneInstruction (executeOneInstruction)
import PythonHS.VM.HandleRuntimeError (handleRuntimeError)
import PythonHS.VM.InitVMState (initVMState)
import PythonHS.VM.Instruction (Instruction (Halt, ReturnTop))
import PythonHS.VM.TraceRunResult (TraceRunResult (..))
import PythonHS.VM.TraceStep (TraceStep (..))
import PythonHS.VM.VMState (VMState (..))

-- Steps tracer for debugging: mirrors RunInstructions while recording one
-- TraceStep snapshot (the state right before executing an instruction) per
-- step into vmTraceSteps, and the first unhandled runtime error into
-- vmTraceFailure instead of aborting with Left. Limits: nested function call
-- frames reset the trace, so their body steps are not part of the caller's
-- trace.
runInstructionsTrace :: [Instruction] -> TraceRunResult
runInstructionsTrace instructions = case vmExecute (initVMState instructions) of
  Right finalState -> toTraceRunResult finalState
  Left err ->
    TraceRunResult {traceRunResultOutputs = [], traceRunResultFailure = Just err, traceRunResultSteps = []}
  where
    vmExecute state
      | vmIp state < 0 || vmIp state >= length (vmCode state) = Right state
      | Just _ <- vmTraceFailure state = Right state
      | otherwise =
          let stepped = state {vmTraceSteps = makeTraceStep state : vmTraceSteps state}
           in case vmCode stepped !! vmIp stepped of
                instruction ->
                  case nextStepResult stepped instruction of
                    Right nextState
                      | instruction == Halt -> finishWithFailure stepped (Right nextState)
                      | instruction == ReturnTop && vmIp nextState == vmIp stepped -> finishWithFailure stepped (Right nextState)
                      | otherwise -> vmExecute nextState
                    Left err -> finishWithFailure stepped (handleRuntimeError vmExecute stepped (Left err))
    nextStepResult stepped _ = case vmCode stepped !! vmIp stepped of
      instruction -> executeOneInstruction vmExecute stepped instruction
    finishWithFailure stepped result = case result of
      Right newState -> Right newState
      Left err ->
        if vmIsTopLevel stepped
          then Right (stepped {vmTraceFailure = Just err})
          else Left err
    makeTraceStep state =
      TraceStep
        { traceStepIp = vmIp state,
          traceStepInstruction = vmCode state !! vmIp state,
          traceStepHandlers = exceptionHandlers (vmException state),
          traceStepStack = vmStack state,
          traceStepOutputs = vmOutputs state
        }
    toTraceRunResult state =
      TraceRunResult
        { traceRunResultOutputs = vmOutputs state,
          traceRunResultFailure = vmTraceFailure state,
          traceRunResultSteps = reverse (vmTraceSteps state)
        }
