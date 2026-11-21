module PythonHS.VM.ExecuteControlFlowInstructionConfig (ExecuteControlFlowInstructionConfig (..)) where

import PythonHS.VM.Instruction (Instruction)
import PythonHS.VM.VMState (VMState)

data ExecuteControlFlowInstructionConfig = ExecuteControlFlowInstructionConfig
  { executeControlFlowInstructionState :: VMState,
    executeControlFlowInstructionInstruction :: Instruction
  }
