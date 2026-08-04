module PythonHS.VM.CompiledDefaultCode (CompiledDefaultCode (..)) where

data CompiledDefaultCode instruction = CompiledDefaultCode
  { compiledDefaultCodeParam :: String,
    compiledDefaultCodeInstructions :: [instruction]
  }
  deriving (Eq, Show)
