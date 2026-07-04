module PythonHS.VM.SliceValue (sliceValue) where

import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.SliceValueConfig (SliceValueConfig (..))

sliceValue :: SliceValueConfig -> Either String Value
sliceValue config = case seqValue of
  ListValue {listValueItems = vals} -> fmap ListValue (sliceValues vals startVal endVal)
  TupleValue {tupleValueItems = vals} -> fmap TupleValue (sliceValues vals startVal endVal)
  StringValue {stringValue = s} -> fmap StringValue (sliceString s startVal endVal)
  _ -> Left ("Type error: slice expects list, tuple, or string as first argument at " ++ showPos pos)
  where
    pos = sliceValuePos config
    seqValue = sliceValueSeqValue config
    startVal = sliceValueStartVal config
    endVal = sliceValueEndVal config
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
        IntValue {intValue = idx} ->
          let adjusted = if idx < 0 then idx + fromIntegral lenNow else idx
           in Right (fromIntegral adjusted)
        _ -> Left ("Type error: slice expects int or None bounds at " ++ showPos pos)
