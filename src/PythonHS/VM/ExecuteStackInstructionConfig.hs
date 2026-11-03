module PythonHS.VM.ExecuteStackInstructionConfig (ExecuteStackInstructionConfig (..)) where

import PythonHS.VM.Instruction (Instruction)
import PythonHS.VM.VMState (VMState)

data ExecuteStackInstructionConfig = ExecuteStackInstructionConfig
  { executeStackInstructionState :: VMState,
    executeStackInstructionInstruction :: Instruction
  }
