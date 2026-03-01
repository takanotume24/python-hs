module PythonHS.VM.IsTruthy (isTruthy) where

import PythonHS.Evaluator.Value (Value (BreakValue, ContinueValue, DictValue, FloatValue, IntValue, ListValue, NoneValue, StringValue))

isTruthy :: Value -> Bool
isTruthy value =
  case value of
    IntValue n -> n /= 0
    FloatValue n -> n /= 0
    StringValue s -> not (null s)
    NoneValue -> False
    ListValue vals -> not (null vals)
    DictValue pairs -> not (null pairs)
    BreakValue -> True
    ContinueValue -> True
