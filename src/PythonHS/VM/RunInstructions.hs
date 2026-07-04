module PythonHS.VM.RunInstructions (runInstructions) where

import PythonHS.VM.ExecuteOneInstruction (executeOneInstruction)
import PythonHS.VM.HandleRuntimeError (handleRuntimeError)
import PythonHS.VM.InitVMState (initVMState)
import PythonHS.VM.Instruction (Instruction)
import PythonHS.VM.VMState (VMState (..))

runInstructions :: [Instruction] -> Either String [String]
runInstructions instructions = do
  finalState <- execute (initVMState instructions)
  pure (vmOutputs finalState)
  where
    execute state
      | vmIp state < 0 || vmIp state >= length (vmCode state) = Right state
      | otherwise = do
          handleRuntimeError execute state $
            case vmCode state !! vmIp state of
              instruction -> executeOneInstruction execute state instruction
