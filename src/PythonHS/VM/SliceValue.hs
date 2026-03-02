module PythonHS.VM.SliceValue (sliceValue) where

import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (IntValue, ListValue, NoneValue, StringValue, TupleValue), Value)
import PythonHS.Lexer.Position (Position)

sliceValue :: Position -> Value -> Value -> Value -> Either String Value
sliceValue pos seqValue startVal endVal =
  case seqValue of
    ListValue vals -> fmap ListValue (sliceValues vals startVal endVal)
    TupleValue vals -> fmap TupleValue (sliceValues vals startVal endVal)
    StringValue s -> fmap StringValue (sliceString s startVal endVal)
    _ -> Left ("Type error: slice expects list, tuple, or string as first argument at " ++ showPos pos)
  where
    sliceValues values startBound endBound = do
      (startIdx, endIdx) <- sliceBounds (length values) startBound endBound
      let sliceLen = max 0 (endIdx - startIdx)
      Right (take sliceLen (drop startIdx values))

    sliceString s startBound endBound = do
      (startIdx, endIdx) <- sliceBounds (length s) startBound endBound
      let sliceLen = max 0 (endIdx - startIdx)
      Right (take sliceLen (drop startIdx s))

    sliceBounds lenNow startBound endBound = do
      startIdx <- normalizeBound 0 lenNow startBound
      endIdx <- normalizeBound lenNow lenNow endBound
      let clampedStart = max 0 (min lenNow startIdx)
          clampedEnd = max 0 (min lenNow endIdx)
      Right (clampedStart, clampedEnd)

    normalizeBound defaultVal lenNow value =
      case value of
        NoneValue -> Right defaultVal
        IntValue idx ->
          let adjusted = if idx < 0 then idx + fromIntegral lenNow else idx
           in Right (fromIntegral adjusted)
        _ -> Left ("Type error: slice expects int or None bounds at " ++ showPos pos)
