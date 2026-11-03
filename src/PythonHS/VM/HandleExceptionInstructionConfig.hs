module PythonHS.VM.HandleExceptionInstructionConfig (HandleExceptionInstructionConfig (..)) where

import PythonHS.VM.Instruction (Instruction)
import PythonHS.VM.VMState (VMState)

data HandleExceptionInstructionConfig = HandleExceptionInstructionConfig
  { handleExceptionInstructionState :: VMState,
    handleExceptionInstructionInstruction :: Instruction
  }
