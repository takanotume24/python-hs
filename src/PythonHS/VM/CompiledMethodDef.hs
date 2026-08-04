module PythonHS.VM.CompiledMethodDef (CompiledMethodDef (..)) where

import PythonHS.VM.CompiledDefaultCode (CompiledDefaultCode)
import PythonHS.VM.Instruction (Instruction)

data CompiledMethodDef = CompiledMethodDef
  { compiledMethodDefName :: String,
    compiledMethodDefParams :: [String],
    compiledMethodDefDefaults :: [CompiledDefaultCode Instruction],
    compiledMethodDefBody :: [Instruction]
  }
  deriving (Eq, Show)
