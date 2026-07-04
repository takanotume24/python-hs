module PythonHS.VM.CallBuiltin (callBuiltin) where

import PythonHS.Evaluator.RangeValues (rangeValues)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.CallCollectionBuiltin (callCollectionBuiltin)
import PythonHS.VM.CallMathBuiltin (callMathBuiltin)
import PythonHS.VM.CallStdlibBuiltin (callStdlibBuiltin)

callBuiltin :: String -> [Value] -> Position -> Maybe (Either String Value)
callBuiltin name args pos =
  case name of
    "len" -> callCollectionBuiltin name args pos
    "bool" -> callCollectionBuiltin name args pos
    "range" -> Just $ case args of
      [IntValue {intValue = n}] -> Right (ListValue {listValueItems = map IntValue (rangeOne n)})
      [IntValue {intValue = start}, IntValue {intValue = stop}] -> Right (ListValue {listValueItems = map IntValue (rangeValues start stop 1)})
      [IntValue {intValue = start}, IntValue {intValue = stop}, IntValue {intValue = step}] ->
        if step == 0
          then Left ("Value error: range step must not be zero at " ++ showPos pos)
          else Right (ListValue {listValueItems = map IntValue (rangeValues start stop step)})
      [_] -> Left ("Type error: range expects int at " ++ showPos pos)
      [_, _] -> Left ("Type error: range expects int arguments at " ++ showPos pos)
      [_, _, _] -> Left ("Type error: range expects int arguments at " ++ showPos pos)
      _ -> Left ("Argument count mismatch when calling range at " ++ showPos pos)
    _ ->
      case callMathBuiltin name args pos of
        Just result -> Just result
        Nothing ->
          case callStdlibBuiltin name args pos of
            Just result -> Just result
            Nothing -> callCollectionBuiltin name args pos
  where
    rangeOne n
      | n <= 0 = []
      | otherwise = [0 .. n - 1]
