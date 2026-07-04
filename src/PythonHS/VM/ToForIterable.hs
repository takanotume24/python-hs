module PythonHS.VM.ToForIterable (toForIterable) where

import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.ToForIterableConfig (ToForIterableConfig (..))

toForIterable :: ToForIterableConfig -> Either String [Value]
toForIterable config =
  let iterableValue = toForIterableValue config
      pos = toForIterablePos config
   in case iterableValue of
        IntValue {intValue = maxN} ->
          let upper = max 0 maxN
           in Right (map IntValue [0 .. upper - 1])
        ListValue {listValueItems = vals} -> Right vals
        TupleValue {tupleValueItems = vals} -> Right vals
        DictValue {dictValuePairs = pairs} -> Right (map fst pairs)
        _ -> Left ("Type error: for expects iterable (int range, list, or dict) at " ++ showPos pos)
