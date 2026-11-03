module PythonHS.VM.TryExceptDispatchPlan (TryExceptDispatchPlan (..)) where

import PythonHS.VM.CompileExprResult (CompileExprResult)
import PythonHS.VM.Instruction (Instruction)

data TryExceptDispatchPlan = TryExceptDispatchPlan
  { tryExceptDispatchPlanResult :: CompileExprResult,
    tryExceptDispatchPlanFinallyStart :: Int,
    tryExceptDispatchPlanFinallyTail :: [Instruction]
  }
