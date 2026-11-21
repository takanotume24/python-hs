module PythonHS.VM.CallBuiltin (callBuiltin) where

import PythonHS.Evaluator.RangeValues (rangeValues)
import PythonHS.Evaluator.RangeValuesConfig (RangeValuesConfig (..))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.CallBuiltinConfig (CallBuiltinConfig (..))
import PythonHS.VM.CallCollectionBuiltin (callCollectionBuiltin)
import PythonHS.VM.CallCollectionBuiltinConfig (CallCollectionBuiltinConfig (..))
import PythonHS.VM.CallConstructorBuiltin (callConstructorBuiltin)
import PythonHS.VM.CallConstructorBuiltinConfig (CallConstructorBuiltinConfig (..))
import PythonHS.VM.CallConversionBuiltin (callConversionBuiltin)
import PythonHS.VM.CallConversionBuiltinConfig (CallConversionBuiltinConfig (..))
import PythonHS.VM.CallIterableBuiltin (callIterableBuiltin)
import PythonHS.VM.CallIterableBuiltinConfig (CallIterableBuiltinConfig (..))
import PythonHS.VM.CallMathBuiltin (callMathBuiltin)
import PythonHS.VM.CallMathBuiltinConfig (CallMathBuiltinConfig (..))
import PythonHS.VM.CallNumericBuiltin (callNumericBuiltin)
import PythonHS.VM.CallNumericBuiltinConfig (CallNumericBuiltinConfig (..))
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
          firstJust
            [ callConstructorBuiltin CallConstructorBuiltinConfig {callConstructorBuiltinName = name, callConstructorBuiltinArgs = args, callConstructorBuiltinPos = pos},
              callConversionBuiltin CallConversionBuiltinConfig {callConversionBuiltinName = name, callConversionBuiltinArgs = args, callConversionBuiltinPos = pos},
              callIterableBuiltin CallIterableBuiltinConfig {callIterableBuiltinName = name, callIterableBuiltinArgs = args, callIterableBuiltinPos = pos},
              callNumericBuiltin CallNumericBuiltinConfig {callNumericBuiltinName = name, callNumericBuiltinArgs = args, callNumericBuiltinPos = pos},
              callMathBuiltin CallMathBuiltinConfig {callMathBuiltinName = name, callMathBuiltinArgs = args, callMathBuiltinPos = pos},
              callStdlibBuiltin CallStdlibBuiltinConfig {callStdlibBuiltinName = name, callStdlibBuiltinArgs = args, callStdlibBuiltinPos = pos},
              callCollectionBuiltin CallCollectionBuiltinConfig {callCollectionBuiltinName = name, callCollectionBuiltinArgs = args, callCollectionBuiltinPos = pos}
            ]
  where
    firstJust [] = Nothing
    firstJust (Just result : _) = Just result
    firstJust (Nothing : restResults) = firstJust restResults

    rangeOne n
      | n <= 0 = []
      | otherwise = [0 .. n - 1]
