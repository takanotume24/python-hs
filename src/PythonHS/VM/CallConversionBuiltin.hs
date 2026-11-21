module PythonHS.VM.CallConversionBuiltin (callConversionBuiltin) where

import Data.Char (chr, isDigit, isSpace, ord)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Evaluator.ValueToOutput (valueToOutput)
import PythonHS.VM.CallConversionBuiltinConfig (CallConversionBuiltinConfig (..))

callConversionBuiltin :: CallConversionBuiltinConfig -> Maybe (Either String Value)
callConversionBuiltin config =
  case name of
    "str" -> Just (evalStr name args)
    "int" -> Just (evalInt name args)
    "float" -> Just (evalFloat name args)
    "chr" -> Just (evalChr name args)
    "ord" -> Just (evalOrd name args)
    _ -> Nothing
  where
    name = callConversionBuiltinName config
    args = callConversionBuiltinArgs config
    pos = callConversionBuiltinPos config

    evalStr fname values =
      case values of
        [value] -> Right (StringValue {stringValue = valueToOutput value})
        _ -> Left (countMismatch fname)

    evalInt fname values =
      case values of
        [IntValue {intValue = n}] -> Right (IntValue {intValue = n})
        [FloatValue {floatValue = n}]
          | isNaN n || isInfinite n ->
              Left ("Value error: cannot convert float to int at " ++ showPos pos)
          | otherwise -> Right (IntValue {intValue = truncate n})
        [StringValue {stringValue = text}] ->
          case parseIntegerText text of
            Just n -> Right (IntValue {intValue = n})
            Nothing -> Left ("Value error: invalid literal for int at " ++ showPos pos)
        [_] -> Left ("Type error: " ++ fname ++ " expects int, float, or string at " ++ showPos pos)
        _ -> Left (countMismatch fname)

    evalFloat fname values =
      case values of
        [IntValue {intValue = n}] -> Right (FloatValue {floatValue = fromIntegral n})
        [FloatValue {floatValue = n}] -> Right (FloatValue {floatValue = n})
        [StringValue {stringValue = text}] ->
          case parseFloatText text of
            Just n -> Right (FloatValue {floatValue = n})
            Nothing -> Left ("Value error: could not convert string to float at " ++ showPos pos)
        [_] -> Left ("Type error: " ++ fname ++ " expects int, float, or string at " ++ showPos pos)
        _ -> Left (countMismatch fname)

    parseIntegerText text =
      case trimmed of
        '+' : digits -> readDigits digits
        '-' : digits -> negate <$> readDigits digits
        digits -> readDigits digits
      where
        trimmed = trimSpace text
        readDigits digits
          | not (null digits) && all isDigit digits = Just (read digits)
          | otherwise = Nothing

    parseFloatText text =
      case reads (trimSpace text) of
        [(n, "")] -> Just n
        _ -> Nothing

    evalChr fname values =
      case values of
        [IntValue {intValue = n}]
          | n < 0 || n > 0x10FFFF -> Left ("Value error: chr() arg not in range at " ++ showPos pos)
          | otherwise -> Right (StringValue {stringValue = [chr (fromIntegral n)]})
        [_] -> Left ("Type error: chr expects int at " ++ showPos pos)
        _ -> Left (countMismatch fname)

    evalOrd fname values =
      case values of
        [StringValue {stringValue = [character]}] -> Right (IntValue {intValue = fromIntegral (ord character)})
        [StringValue {}] -> Left ("Type error: ord() expected a character at " ++ showPos pos)
        [_] -> Left ("Type error: ord() expected a character at " ++ showPos pos)
        _ -> Left (countMismatch fname)

    trimSpace = reverse . dropWhile isSpace . reverse . dropWhile isSpace

    countMismatch fname = "Argument count mismatch when calling " ++ fname ++ " at " ++ showPos pos
