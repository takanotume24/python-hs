module PythonHS.VM.ExecuteStackInstruction (executeStackInstruction) where

import PythonHS.Evaluator.ValueToOutput (valueToOutput)
import PythonHS.VM.ExecuteStackInstructionConfig (ExecuteStackInstructionConfig (..))
import PythonHS.VM.Instruction (Instruction (..))
import PythonHS.VM.VMState (VMState (..))

executeStackInstruction :: ExecuteStackInstructionConfig -> Either String VMState
executeStackInstruction config =
  let execute = executeStackInstructionExecute config
      state = executeStackInstructionState config
   in case executeStackInstructionInstruction config of
        PrintTop ->
          case vmStack state of
            value : rest -> execute state {vmIp = vmIp state + 1, vmStack = rest, vmOutputs = vmOutputs state ++ [valueToOutput value]}
            _ -> Left "VM runtime error: print requires one value on stack"
        PopTop ->
          case vmStack state of
            _ : rest -> execute state {vmIp = vmIp state + 1, vmStack = rest}
            [] -> Left "VM runtime error: pop requires one value on stack"
        _ -> Left "VM runtime error: unexpected instruction in executeStackInstruction"
