module PythonHS.VM.ExecuteArithmeticInstruction (executeArithmeticInstruction) where

import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (FloatValue, IntValue))
import PythonHS.VM.EvalBinaryOp (evalBinaryOp)
import PythonHS.VM.Instruction (Instruction (..))
import PythonHS.VM.IsTruthy (isTruthy)
import PythonHS.VM.VMState (VMState (..))

executeArithmeticInstruction :: (VMState -> Either String VMState) -> VMState -> Instruction -> Maybe (Either String VMState)
executeArithmeticInstruction execute state instruction =
  case instruction of
    ApplyBinary op pos ->
      case vmStack state of
        right : left : rest ->
          case evalBinaryOp op left right pos of
            Left err -> Just (Left err)
            Right value -> Just (execute state {vmIp = vmIp state + 1, vmStack = value : rest})
        _ -> Just (Left "VM runtime error: add requires two values on stack")
    ApplyUnaryMinus pos ->
      case vmStack state of
        value : rest ->
          case value of
            IntValue n -> Just (execute state {vmIp = vmIp state + 1, vmStack = IntValue (negate n) : rest})
            FloatValue n -> Just (execute state {vmIp = vmIp state + 1, vmStack = FloatValue (negate n) : rest})
            _ -> Just (Left ("Type error: unary - expects int at " ++ showPos pos))
        _ -> Just (Left "VM runtime error: unary - requires one value on stack")
    ApplyNot _ ->
      case vmStack state of
        value : rest -> Just (execute state {vmIp = vmIp state + 1, vmStack = IntValue (if isTruthy value then 0 else 1) : rest})
        _ -> Just (Left "VM runtime error: not requires one value on stack")
    _ -> Nothing
