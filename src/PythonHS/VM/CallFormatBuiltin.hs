module PythonHS.VM.CallFormatBuiltin (callFormatBuiltin) where

import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.CallBuiltinConfig (CallBuiltinConfig (..))
import PythonHS.VM.FormatValue (formatValue)
import PythonHS.VM.FormatValueConfig (FormatValueConfig (..))

callFormatBuiltin :: CallBuiltinConfig -> Maybe (Either String Value)
callFormatBuiltin config =
  case (callBuiltinName config, callBuiltinArgs config) of
    ("__python_hs_format__", [value, StringValue {stringValue = formatSpec}]) ->
      Just
        ( fmap
            (\text -> StringValue {stringValue = text})
            (formatValue FormatValueConfig {formatValueValue = value, formatValueSpec = formatSpec, formatValuePos = pos})
        )
    ("__python_hs_format__", _) ->
      Just (Left ("Argument count mismatch when calling __python_hs_format__ at " ++ showPos pos))
    _ -> Nothing
  where
    pos = callBuiltinPos config
