module PythonHS.VM.CallBuiltin (callBuiltin) where

import PythonHS.Evaluator.RangeValues (rangeValues)
import PythonHS.Evaluator.RangeValuesConfig (RangeValuesConfig (..))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.CallBuiltinConfig (CallBuiltinConfig (..))
import PythonHS.VM.CallCollectionBuiltin (callCollectionBuiltin)
import PythonHS.VM.CallCollectionBuiltinConfig (CallCollectionBuiltinConfig (..))
import PythonHS.VM.CallMathBuiltin (callMathBuiltin)
import PythonHS.VM.CallMathBuiltinConfig (CallMathBuiltinConfig (..))
import PythonHS.VM.CallStdlibBuiltin (callStdlibBuiltin)
import PythonHS.VM.CallStdlibBuiltinConfig (CallStdlibBuiltinConfig (..))

callBuiltin :: CallBuiltinConfig -> Maybe (Either String Value)
callBuiltin config =
  let name = callBuiltinName config
      args = callBuiltinArgs config
      pos = callBuiltinPos config
   in case name of
        "len" -> callCollectionBuiltin CallCollectionBuiltinConfig {callCollectionBuiltinName = name, callCollectionBuiltinArgs = args, callCollectionBuiltinPos = pos}
        "bool" -> callCollectionBuiltin CallCollectionBuiltinConfig {callCollectionBuiltinName = name, callCollectionBuiltinArgs = args, callCollectionBuiltinPos = pos}
        "range" -> Just $ case args of
          [IntValue {intValue = n}] -> Right (ListValue {listValueItems = map IntValue (rangeOne n)})
          [IntValue {intValue = start}, IntValue {intValue = stop}] -> Right (ListValue {listValueItems = map IntValue (rangeValues RangeValuesConfig {rangeValuesStart = start, rangeValuesStop = stop, rangeValuesStep = 1})})
          [IntValue {intValue = start}, IntValue {intValue = stop}, IntValue {intValue = step}] ->
            if step == 0
              then Left ("Value error: range step must not be zero at " ++ showPos pos)
              else Right (ListValue {listValueItems = map IntValue (rangeValues RangeValuesConfig {rangeValuesStart = start, rangeValuesStop = stop, rangeValuesStep = step})})
          [_] -> Left ("Type error: range expects int at " ++ showPos pos)
          [_, _] -> Left ("Type error: range expects int arguments at " ++ showPos pos)
          [_, _, _] -> Left ("Type error: range expects int arguments at " ++ showPos pos)
          _ -> Left ("Argument count mismatch when calling range at " ++ showPos pos)
        _ ->
          case callMathBuiltin CallMathBuiltinConfig {callMathBuiltinName = name, callMathBuiltinArgs = args, callMathBuiltinPos = pos} of
            Just result -> Just result
            Nothing ->
              case callStdlibBuiltin CallStdlibBuiltinConfig {callStdlibBuiltinName = name, callStdlibBuiltinArgs = args, callStdlibBuiltinPos = pos} of
                Just result -> Just result
                Nothing -> callCollectionBuiltin CallCollectionBuiltinConfig {callCollectionBuiltinName = name, callCollectionBuiltinArgs = args, callCollectionBuiltinPos = pos}
  where
    rangeOne n
      | n <= 0 = []
      | otherwise = [0 .. n - 1]
