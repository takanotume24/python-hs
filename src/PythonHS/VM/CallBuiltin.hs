module PythonHS.VM.CallBuiltin (callBuiltin) where

import PythonHS.Evaluator.RangeValues (rangeValues)
import PythonHS.Evaluator.RangeValuesConfig (RangeValuesConfig (..))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.CallBuiltinConfig (CallBuiltinConfig (..))
import PythonHS.VM.CallCollectionBuiltin (callCollectionBuiltin)
import PythonHS.VM.CallCollectionBuiltinConfig (CallCollectionBuiltinConfig (..))
import PythonHS.VM.CallCollectionMethodBuiltin (callCollectionMethodBuiltin)
import PythonHS.VM.CallCollectionMethodBuiltinConfig (CallCollectionMethodBuiltinConfig (..))
import PythonHS.VM.CallCompileBuiltin (callCompileBuiltin)
import PythonHS.VM.CallConstructorBuiltin (callConstructorBuiltin)
import PythonHS.VM.CallConstructorBuiltinConfig (CallConstructorBuiltinConfig (..))
import PythonHS.VM.CallConversionBuiltin (callConversionBuiltin)
import PythonHS.VM.CallConversionBuiltinConfig (CallConversionBuiltinConfig (..))
import PythonHS.VM.CallFormatBuiltin (callFormatBuiltin)
import PythonHS.VM.CallInputBuiltin (callInputBuiltin)
import PythonHS.VM.CallIterableBuiltin (callIterableBuiltin)
import PythonHS.VM.CallIterableBuiltinConfig (CallIterableBuiltinConfig (..))
import PythonHS.VM.CallMathBuiltin (callMathBuiltin)
import PythonHS.VM.CallMathBuiltinConfig (CallMathBuiltinConfig (..))
import PythonHS.VM.CallNumericBuiltin (callNumericBuiltin)
import PythonHS.VM.CallNumericBuiltinConfig (CallNumericBuiltinConfig (..))
import PythonHS.VM.CallNumericExtraBuiltin (callNumericExtraBuiltin)
import PythonHS.VM.CallStdlibBuiltin (callStdlibBuiltin)
import PythonHS.VM.CallStdlibBuiltinConfig (CallStdlibBuiltinConfig (..))
import PythonHS.VM.CallStringBuiltin (callStringBuiltin)
import PythonHS.VM.CallStringBuiltinConfig (CallStringBuiltinConfig (..))
import PythonHS.VM.CallStringPredicateBuiltin (callStringPredicateBuiltin)
import PythonHS.VM.CallStringSearchBuiltin (callStringSearchBuiltin)
import PythonHS.VM.CallUnittestBuiltin (callUnittestBuiltin)
import PythonHS.VM.CallUnittestBuiltinConfig (CallUnittestBuiltinConfig (..))

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
              callFormatBuiltin CallBuiltinConfig {callBuiltinName = name, callBuiltinArgs = args, callBuiltinPos = pos},
              callIterableBuiltin CallIterableBuiltinConfig {callIterableBuiltinName = name, callIterableBuiltinArgs = args, callIterableBuiltinPos = pos},
              callStringBuiltin CallStringBuiltinConfig {callStringBuiltinName = name, callStringBuiltinArgs = args, callStringBuiltinPos = pos},
              callStringSearchBuiltin CallStringBuiltinConfig {callStringBuiltinName = name, callStringBuiltinArgs = args, callStringBuiltinPos = pos},
              callStringPredicateBuiltin CallStringBuiltinConfig {callStringBuiltinName = name, callStringBuiltinArgs = args, callStringBuiltinPos = pos},
              callCollectionMethodBuiltin CallCollectionMethodBuiltinConfig {callCollectionMethodBuiltinName = name, callCollectionMethodBuiltinArgs = args, callCollectionMethodBuiltinPos = pos},
              callNumericBuiltin CallNumericBuiltinConfig {callNumericBuiltinName = name, callNumericBuiltinArgs = args, callNumericBuiltinPos = pos},
              callNumericExtraBuiltin CallBuiltinConfig {callBuiltinName = name, callBuiltinArgs = args, callBuiltinPos = pos},
              callMathBuiltin CallMathBuiltinConfig {callMathBuiltinName = name, callMathBuiltinArgs = args, callMathBuiltinPos = pos},
              callStdlibBuiltin CallStdlibBuiltinConfig {callStdlibBuiltinName = name, callStdlibBuiltinArgs = args, callStdlibBuiltinPos = pos},
              callCompileBuiltin CallBuiltinConfig {callBuiltinName = name, callBuiltinArgs = args, callBuiltinPos = pos},
              callCollectionBuiltin CallCollectionBuiltinConfig {callCollectionBuiltinName = name, callCollectionBuiltinArgs = args, callCollectionBuiltinPos = pos},
              callUnittestBuiltin CallUnittestBuiltinConfig {callUnittestBuiltinName = name, callUnittestBuiltinArgs = args, callUnittestBuiltinPos = pos},
              callInputBuiltin CallBuiltinConfig {callBuiltinName = name, callBuiltinArgs = args, callBuiltinPos = pos}
            ]
  where
    firstJust [] = Nothing
    firstJust (Just result : _) = Just result
    firstJust (Nothing : restResults) = firstJust restResults

    rangeOne n
      | n <= 0 = []
      | otherwise = [0 .. n - 1]
