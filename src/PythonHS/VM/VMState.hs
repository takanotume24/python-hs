module PythonHS.VM.VMState (VMState (..)) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.VM.EnvState (EnvState (..))
import PythonHS.VM.ExceptionState (ExceptionState (..))
import PythonHS.VM.Instruction (Instruction)
import PythonHS.VM.LoopState (LoopState (..))

data VMState = VMState
  { vmCode :: [Instruction],
    vmIp :: Int,
    vmStack :: [Value],
    vmEnv :: EnvState,
    vmLoop :: LoopState,
    vmException :: ExceptionState,
    vmIsTopLevel :: Bool,
    vmOutputs :: [String]
  }
  deriving (Show)
