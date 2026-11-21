module PythonHS.VM.CompileComparisonChainBodyResult (CompileComparisonChainBodyResult (..)) where

import PythonHS.VM.Instruction (Instruction)

data CompileComparisonChainBodyResult = CompileComparisonChainBodyResult
  { compileComparisonChainBodyCode :: [Instruction],
    compileComparisonChainBodyEndIndex :: Int
  }
