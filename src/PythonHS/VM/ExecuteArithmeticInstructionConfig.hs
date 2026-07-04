module PythonHS.VM.ExecuteArithmeticInstructionConfig (ExecuteArithmeticInstructionConfig (..)) where

import PythonHS.VM.Instruction (Instruction)
import PythonHS.VM.VMState (VMState)

data ExecuteArithmeticInstructionConfig = ExecuteArithmeticInstructionConfig
  { executeArithmeticInstructionExecute :: VMState -> Either String VMState,
    executeArithmeticInstructionState :: VMState,
    executeArithmeticInstructionInstruction :: Instruction
  }
