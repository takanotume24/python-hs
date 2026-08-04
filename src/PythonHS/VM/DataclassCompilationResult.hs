module PythonHS.VM.DataclassCompilationResult (DataclassCompilationResult (..)) where

import PythonHS.VM.Instruction (Instruction)
import PythonHS.VM.MethodNamePair (MethodNamePair)

data DataclassCompilationResult = DataclassCompilationResult
  { dataclassCompilationResultCode :: [Instruction],
    dataclassCompilationResultPairs :: [MethodNamePair],
    dataclassCompilationResultCount :: Int
  }
  deriving (Eq, Show)
