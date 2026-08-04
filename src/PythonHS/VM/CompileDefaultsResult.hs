module PythonHS.VM.CompileDefaultsResult (CompileDefaultsResult (..)) where

import PythonHS.VM.CompiledDefaultCode (CompiledDefaultCode)
import PythonHS.VM.Instruction (Instruction)

data CompileDefaultsResult = CompileDefaultsResult
  { compileDefaultsResultDefaults :: [CompiledDefaultCode Instruction],
    compileDefaultsResultEndIndex :: Int
  }
  deriving (Eq, Show)
