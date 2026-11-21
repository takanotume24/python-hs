module PythonHS.Evaluator.ExpectTruthy (expectTruthy) where

import PythonHS.Evaluator.ExpectTruthyInput (ExpectTruthyInput (..))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))

expectTruthy :: ExpectTruthyInput -> Either String Int
expectTruthy input = case expectTruthyInputValue input of
  IntValue {intValue = n} -> Right (if n == 0 then 0 else 1)
  FloatValue {floatValue = n} -> Right (if n == 0 then 0 else 1)
  NoneValue -> Right 0
  StringValue {stringValue = s} -> Right (if null s then 0 else 1)
  ListValue {listValueItems = vals} -> Right (if null vals then 0 else 1)
  TupleValue {tupleValueItems = vals} -> Right (if null vals then 0 else 1)
  DictValue {dictValuePairs = pairs} -> Right (if null pairs then 0 else 1)
  _ -> Left $ "Type error: expected int in " ++ expectTruthyInputContext input ++ " at " ++ showPos (expectTruthyInputPos input)
