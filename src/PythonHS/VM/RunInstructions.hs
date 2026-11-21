module PythonHS.VM.RunInstructions (runInstructions) where

import PythonHS.VM.ExecuteOneInstruction (executeOneInstruction)
import PythonHS.VM.HandleRuntimeError (handleRuntimeError)
import PythonHS.VM.InitVMState (initVMState)
import PythonHS.VM.Instruction (Instruction (Halt, ReturnTop))
import PythonHS.VM.VMState (VMState (..))

runInstructions :: [Instruction] -> Either String [String]
runInstructions instructions = do
  finalState <- execute (initVMState instructions)
  pure (vmOutputs finalState)
  where
    execute state
      | vmIp state < 0 || vmIp state >= length (vmCode state) = Right state
      | otherwise =
          case vmCode state !! vmIp state of
            instruction ->
              case executeOneInstruction execute state instruction of
                Right nextState
                  | instruction == Halt -> Right nextState
                  | instruction == ReturnTop && vmIp nextState == vmIp state -> Right nextState
                  | otherwise -> execute nextState
                Left err -> handleRuntimeError execute state (Left err)
