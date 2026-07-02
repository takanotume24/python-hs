module PythonHS.VM.IsTruthy (isTruthy) where

import PythonHS.Evaluator.Value (Value (..))

isTruthy :: Value -> Bool
isTruthy value =
  case value of
    IntValue {intValue = n} -> n /= 0
    FloatValue {floatValue = n} -> n /= 0
    StringValue {stringValue = s} -> not (null s)
    NoneValue -> False
    ListValue {listValueItems = vals} -> not (null vals)
    TupleValue {tupleValueItems = vals} -> not (null vals)
    DictValue {dictValuePairs = pairs} -> not (null pairs)
    ModuleValue {} -> True
    ClassValue {} -> True
    FunctionRefValue {} -> True
    InstanceValue {} -> True
    BreakValue -> True
    ContinueValue -> True
