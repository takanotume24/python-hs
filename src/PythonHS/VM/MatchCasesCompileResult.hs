module PythonHS.VM.MatchCasesCompileResult (MatchCasesCompileResult (..)) where

import PythonHS.VM.Instruction (Instruction)

data MatchCasesCompileResult = MatchCasesCompileResult
  { matchCasesCompileResultCode :: [Instruction],
    matchCasesCompileResultEndIndex :: Int
  }
  deriving (Eq, Show)
