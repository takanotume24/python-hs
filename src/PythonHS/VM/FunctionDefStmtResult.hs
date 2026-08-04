module PythonHS.VM.FunctionDefStmtResult (FunctionDefStmtResult (..)) where

import PythonHS.VM.CompiledDefaultCode (CompiledDefaultCode)
import PythonHS.VM.Instruction (Instruction)

data FunctionDefStmtResult = FunctionDefStmtResult
  { functionDefStmtResultCode :: [Instruction],
    functionDefStmtResultDefaults :: [CompiledDefaultCode Instruction]
  }
  deriving (Eq, Show)
