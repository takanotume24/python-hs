module PythonHS.VM.FunctionInfo (FunctionInfo (..)) where

import PythonHS.VM.CompiledDefaultCode (CompiledDefaultCode)
import PythonHS.VM.Instruction (Instruction)

data FunctionInfo = FunctionInfo
  { functionInfoParams :: [String],
    functionInfoDefaults :: [CompiledDefaultCode Instruction],
    functionInfoCode :: [Instruction]
  }
  deriving (Eq, Show)
