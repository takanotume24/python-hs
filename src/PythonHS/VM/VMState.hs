module PythonHS.VM.VMState (VMState (..)) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.VM.EnvState (EnvState (..))
import PythonHS.VM.ExceptionState (ExceptionState (..))
import PythonHS.VM.Instruction (Instruction)
import PythonHS.VM.LoopState (LoopState (..))
import PythonHS.VM.TraceStep (TraceStep)

-- VM runtime state. The vmTrace* fields are only populated by the step tracer
-- (RunInstructionsTrace); plain execution keeps them empty. Traced function
-- call frames start their own fresh trace, so nested body steps are not part
-- of the caller's trace.
data VMState = VMState
  { vmCode :: [Instruction],
    vmIp :: Int,
    vmStack :: [Value],
    vmEnv :: EnvState,
    vmLoop :: LoopState,
    vmException :: ExceptionState,
    vmIsTopLevel :: Bool,
    vmOutputs :: [String],
    vmTraceSteps :: [TraceStep],
    vmTraceFailure :: Maybe String
  }
  deriving (Show)
