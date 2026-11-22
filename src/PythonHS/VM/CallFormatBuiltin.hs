module PythonHS.VM.CallFormatBuiltin (callFormatBuiltin) where

import Data.Char (isDigit)
import Numeric (showFFloat)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Evaluator.ValueToOutput (valueToOutput)
import PythonHS.VM.CallBuiltinConfig (CallBuiltinConfig (..))

callFormatBuiltin :: CallBuiltinConfig -> Maybe (Either String Value)
callFormatBuiltin config =
  case (callBuiltinName config, callBuiltinArgs config) of
    ("__python_hs_format__", [value, StringValue {stringValue = formatSpec}]) ->
      Just (fmap (\text -> StringValue {stringValue = text}) (formatValue value formatSpec))
    ("__python_hs_format__", _) ->
      Just (Left ("Argument count mismatch when calling __python_hs_format__ at " ++ showPos pos))
    _ -> Nothing
  where
    pos = callBuiltinPos config

    formatValue value formatSpec =
      case formatSpec of
        "" -> Right (valueToOutput value)
        "s" -> Right (valueToOutput value)
        "d" ->
          case value of
            IntValue {intValue = n} -> Right (show n)
            _ -> Left ("Type error: d format expects int at " ++ showPos pos)
        _ ->
          case floatPrecision formatSpec of
            Just precision ->
              case numericValue value of
                Just number -> Right (showFFloat precision number "")
                Nothing -> Left ("Type error: f format expects number at " ++ showPos pos)
            Nothing -> Left ("Value error: unsupported format spec " ++ formatSpec ++ " at " ++ showPos pos)

    floatPrecision formatSpec =
      case formatSpec of
        "f" -> Just (Just 6)
        '.' : digits -> case reverse digits of
          'f' : precisionDigits
            | not (null precisionDigits) && all isDigit precisionDigits -> Just (Just (read (reverse precisionDigits)))
          _ -> Nothing
        _ -> Nothing

    numericValue value =
      case value of
        IntValue {intValue = n} -> Just (fromIntegral n)
        FloatValue {floatValue = n} -> Just n
        _ -> Nothing
