module PythonHS.VM.EvalBinaryOpEquality (evalBinaryOpEquality) where

import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Evaluator.ValuesEqual (valuesEqual)
import PythonHS.VM.EvalBinaryOpEqualityConfig (EvalBinaryOpEqualityConfig (..))

evalBinaryOpEquality :: EvalBinaryOpEqualityConfig -> Either String Value
evalBinaryOpEquality config =
  let left = evalBinaryOpEqualityConfigLeft config
      right = evalBinaryOpEqualityConfigRight config
      negateResult = evalBinaryOpEqualityConfigNegate config
      fallback = Right (IntValue {intValue = if (left == right) /= negateResult then 1 else 0})
   in case left of
        IntValue {intValue = leftInt} ->
          case right of
            FloatValue {floatValue = rightFloat} -> Right (IntValue {intValue = if ((fromIntegral leftInt :: Double) == rightFloat) /= negateResult then 1 else 0})
            _ -> fallback
        FloatValue {floatValue = leftFloat} ->
          case right of
            IntValue {intValue = rightInt} -> Right (IntValue {intValue = if (leftFloat == (fromIntegral rightInt :: Double)) /= negateResult then 1 else 0})
            _ -> fallback
        TupleValue {tupleValueItems = leftVals} ->
          case right of
            TupleValue {tupleValueItems = rightVals} -> Right (IntValue {intValue = if (leftVals == rightVals) /= negateResult then 1 else 0})
            _ -> fallback
        SetValue {setValueItems = leftItems} ->
          case right of
            SetValue {setValueItems = rightItems} -> Right (IntValue {intValue = if (setsEqual leftItems rightItems) /= negateResult then 1 else 0})
            _ -> fallback
        BytesValue {bytesValueItems = leftItems} ->
          case right of
            BytesValue {bytesValueItems = rightItems} -> Right (IntValue {intValue = if (leftItems == rightItems) /= negateResult then 1 else 0})
            _ -> fallback
        _ -> fallback
  where
    setsEqual leftItems rightItems =
      length leftItems == length rightItems
        && all (\leftItem -> any (valuesEqual leftItem) rightItems) leftItems
