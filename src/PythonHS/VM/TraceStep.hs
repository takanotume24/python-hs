module PythonHS.VM.TraceStep (TraceStep (..)) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.VM.Instruction (Instruction)

-- Snapshot of executing each instruction in the VM (for debugging)
data TraceStep = TraceStep
  { traceStepIp :: Int,
    traceStepInstruction :: Instruction,
    traceStepHandlers :: [Int],
    traceStepStack :: [Value],
    traceStepOutputs :: [String]
  }
  deriving (Eq, Show)
