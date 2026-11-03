module PythonHS.VM.TraceRunResult (TraceRunResult (..)) where

import PythonHS.VM.TraceStep (TraceStep)

-- Final report of the VM step tracer: outputs, optional runtime failure and
-- the ordered per-instruction steps
data TraceRunResult = TraceRunResult
  { traceRunResultOutputs :: [String],
    traceRunResultFailure :: Maybe String,
    traceRunResultSteps :: [TraceStep]
  }
  deriving (Eq, Show)
