module PythonHS.VM.CallBytesBuiltin (callBytesBuiltin) where

import Data.Char (intToDigit)
import PythonHS.Evaluator.DecodeText (decodeText)
import PythonHS.Evaluator.DictPair (DictPair (..))
import PythonHS.Evaluator.EncodeText (encodeText)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.CallBytesBuiltinConfig (CallBytesBuiltinConfig (..))

callBytesBuiltin :: CallBytesBuiltinConfig -> Maybe (Either String Value)
callBytesBuiltin config =
  case name of
    "bytes" -> Just (constructorBytes False)
    "bytearray" -> Just (constructorBytes True)
    "decode" -> Just decodeValue
    "encode" -> Just encodeValue
    "hex" -> Just hexValue
    _ -> Nothing
  where
    name = callBytesBuiltinName config
    args = callBytesBuiltinArgs config
    pos = callBytesBuiltinPos config

    constructorBytes isMutable =
      case args of
        [] -> Right (makeBytes isMutable [])
        [IntValue {intValue = count}]
          | count < 0 -> Left ("Value error: negative count at " ++ showPos pos)
          | otherwise -> Right (makeBytes isMutable (replicate (fromIntegral count) 0))
        [StringValue {stringValue = text}] -> do
          items <- encodeText text "utf-8"
          Right (makeBytes isMutable items)
        [StringValue {stringValue = text}, StringValue {stringValue = encoding}] -> do
          items <- encodeText text encoding
          Right (makeBytes isMutable items)
        [BytesValue {bytesValueItems = items}] -> Right (makeBytes isMutable items)
        [value] -> do
          items <- intItems value
          Right (makeBytes isMutable items)
        _ -> Left ("Argument count mismatch when calling " ++ name ++ " at " ++ showPos pos)

    intItems value =
      case value of
        ListValue {listValueItems = items} -> convertInts items
        TupleValue {tupleValueItems = items} -> convertInts items
        SetValue {setValueItems = items} -> convertInts items
        DictValue {dictValuePairs = pairs} -> convertInts (map dictPairKey pairs)
        _ -> Left ("Type error: " ++ name ++ " expects iterable of int at " ++ showPos pos)

    convertInts values = mapM convertInt values

    convertInt value =
      case value of
        IntValue {intValue = int}
          | int >= 0 && int <= 255 -> Right (fromIntegral int)
          | otherwise -> Left ("Value error: byte must be in range(0, 256) at " ++ showPos pos)
        _ -> Left ("Type error: " ++ name ++ " expects iterable of int at " ++ showPos pos)

    decodeValue =
      case args of
        [BytesValue {bytesValueItems = items}] -> decodeItems items "utf-8"
        [BytesValue {bytesValueItems = items}, StringValue {stringValue = encoding}] -> decodeItems items encoding
        [BytesValue {}, _] -> Left ("Type error: decode expects str encoding at " ++ showPos pos)
        [_] -> Left ("Type error: decode expects bytes at " ++ showPos pos)
        _ -> Left ("Argument count mismatch when calling decode at " ++ showPos pos)

    decodeItems items encoding = do
      text <- decodeText items encoding
      Right (StringValue {stringValue = text})

    encodeValue =
      case args of
        [StringValue {stringValue = text}] -> encodeItems text "utf-8"
        [StringValue {stringValue = text}, StringValue {stringValue = encoding}] -> encodeItems text encoding
        [StringValue {}, _] -> Left ("Type error: encode expects str encoding at " ++ showPos pos)
        [_] -> Left ("Type error: encode expects str at " ++ showPos pos)
        _ -> Left ("Argument count mismatch when calling encode at " ++ showPos pos)

    encodeItems text encoding = do
      items <- encodeText text encoding
      Right (BytesValue {bytesValueItems = items, bytesValueIsMutable = False})

    hexValue =
      case args of
        [BytesValue {bytesValueItems = items}] -> Right (StringValue {stringValue = concatMap hexPair items})
        [_] -> Left ("Type error: hex expects bytes at " ++ showPos pos)
        _ -> Left ("Argument count mismatch when calling hex at " ++ showPos pos)

    hexPair byte =
      let high = fromIntegral byte `div` 16
          low = fromIntegral byte `mod` 16
       in [intToDigit high, intToDigit low]

    makeBytes isMutable items = BytesValue {bytesValueItems = items, bytesValueIsMutable = isMutable}
