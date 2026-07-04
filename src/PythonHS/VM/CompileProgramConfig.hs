module PythonHS.VM.CompileProgramConfig (CompileProgramConfig (..)) where

import PythonHS.VM.Instruction (Instruction)

data CompileProgramConfig = CompileProgramConfig
  { compileProgramInstructions :: [Instruction]
  }
