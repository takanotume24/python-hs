module PythonHS.VM.ExecuteAssertInstruction (executeAssertInstruction) where

import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.VM.ExecuteAssertInstructionConfig (ExecuteAssertInstructionConfig (..))
import PythonHS.VM.Instruction (Instruction (AssertTop))
import PythonHS.VM.IsTruthy (isTruthy)
import PythonHS.VM.VMState (VMState (..))

executeAssertInstruction :: ExecuteAssertInstructionConfig -> Either String VMState
executeAssertInstruction config =
  case executeAssertInstructionInstruction config of
    AssertTop pos ->
      case vmStack (executeAssertInstructionState config) of
        value : rest ->
          if isTruthy value
            then executeAssertInstructionExecute config (executeAssertInstructionState config) {vmIp = vmIp (executeAssertInstructionState config) + 1, vmStack = rest}
            else Left ("AssertionError: assertion failed at " ++ showPos pos)
        [] -> Left "VM runtime error: assert requires one value on stack"
    _ -> Left "VM runtime error: unexpected instruction in executeAssertInstruction"
