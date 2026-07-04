module PythonHS.VM.ExecuteArithmeticInstruction (executeArithmeticInstruction) where

import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.EvalBinaryOp (evalBinaryOp)
import PythonHS.VM.EvalBinaryOpConfig (EvalBinaryOpConfig (..))
import PythonHS.VM.ExecuteArithmeticInstructionConfig (ExecuteArithmeticInstructionConfig (..))
import PythonHS.VM.Instruction (Instruction (..))
import PythonHS.VM.IsTruthy (isTruthy)
import PythonHS.VM.VMState (VMState (..))

executeArithmeticInstruction :: ExecuteArithmeticInstructionConfig -> Maybe (Either String VMState)
executeArithmeticInstruction config =
  let execute = executeArithmeticInstructionExecute config
      state = executeArithmeticInstructionState config
      instruction = executeArithmeticInstructionInstruction config
   in case instruction of
        ApplyBinary op pos ->
          case vmStack state of
            right : left : rest ->
              case evalBinaryOp EvalBinaryOpConfig {evalBinaryOpOp = op, evalBinaryOpLeft = left, evalBinaryOpRight = right, evalBinaryOpPos = pos} of
                Left err -> Just (Left err)
                Right value -> Just (execute state {vmIp = vmIp state + 1, vmStack = value : rest})
            _ -> Just (Left "VM runtime error: add requires two values on stack")
        ApplyUnaryMinus pos ->
          case vmStack state of
            value : rest ->
              case value of
                IntValue {intValue = n} -> Just (execute state {vmIp = vmIp state + 1, vmStack = IntValue {intValue = negate n} : rest})
                FloatValue {floatValue = n} -> Just (execute state {vmIp = vmIp state + 1, vmStack = FloatValue {floatValue = negate n} : rest})
                _ -> Just (Left ("Type error: unary - expects int at " ++ showPos pos))
            _ -> Just (Left "VM runtime error: unary - requires one value on stack")
        ApplyNot _ ->
          case vmStack state of
            value : rest -> Just (execute state {vmIp = vmIp state + 1, vmStack = IntValue {intValue = if isTruthy value then 0 else 1} : rest})
            _ -> Just (Left "VM runtime error: not requires one value on stack")
        _ -> Nothing
