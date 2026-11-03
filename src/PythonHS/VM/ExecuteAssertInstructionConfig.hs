module PythonHS.VM.ExecuteAssertInstructionConfig (ExecuteAssertInstructionConfig (..)) where

import PythonHS.VM.Instruction (Instruction)
import PythonHS.VM.VMState (VMState)

data ExecuteAssertInstructionConfig = ExecuteAssertInstructionConfig
  { executeAssertInstructionState :: VMState,
    executeAssertInstructionInstruction :: Instruction
  }
