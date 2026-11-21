module PythonHS.VM.IterableItems (iterableItems) where

import PythonHS.Evaluator.DictPair (DictPair (..))
import PythonHS.Evaluator.Value (Value (..))

iterableItems :: Value -> Maybe [Value]
iterableItems value =
  case value of
    ListValue {listValueItems = items} -> Just items
    TupleValue {tupleValueItems = items} -> Just items
    StringValue {stringValue = text} -> Just (map charValue text)
    DictValue {dictValuePairs = pairs} -> Just (map dictPairKey pairs)
    _ -> Nothing
  where
    charValue character = StringValue {stringValue = [character]}
