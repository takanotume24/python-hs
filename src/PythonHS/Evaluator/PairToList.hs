module PythonHS.Evaluator.PairToList (pairToList) where

import PythonHS.Evaluator.Value (Value (ListValue), listValueItems)

pairToList :: (Value, Value) -> Value
pairToList (k, v) = ListValue {listValueItems = [k, v]}
