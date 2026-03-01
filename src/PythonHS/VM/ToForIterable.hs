module PythonHS.VM.ToForIterable (toForIterable) where

import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (DictValue, IntValue, ListValue), Value)
import PythonHS.Lexer.Position (Position)

toForIterable :: Value -> Position -> Either String [Value]
toForIterable iterableValue pos =
  case iterableValue of
    IntValue maxN ->
      let upper = max 0 maxN
       in Right (map IntValue [0 .. upper - 1])
    ListValue vals -> Right vals
    DictValue pairs -> Right (map fst pairs)
    _ -> Left ("Type error: for expects iterable (int range, list, or dict) at " ++ showPos pos)
