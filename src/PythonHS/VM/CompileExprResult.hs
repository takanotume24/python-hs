module PythonHS.VM.CompileExprResult (CompileExprResult (..)) where

import PythonHS.VM.Instruction (Instruction)

-- | Result of compiling an expression into VM instructions.
data CompileExprResult = CompileExprResult
  { compileExprResultCode :: [Instruction],
    compileExprResultEndIndex :: Int
  }
