module PythonHS.VM.SliceRange (sliceRange) where

import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.SliceRangeConfig (SliceRangeConfig (..))
import PythonHS.VM.SliceRangeResult (SliceRangeResult (..))

-- | Normalises slice bounds for a sequence of the given length.
-- The result end is never smaller than the result start so callers can use
-- @take start@ and @drop end@ to replace or delete a slice.
sliceRange :: SliceRangeConfig -> Either String SliceRangeResult
sliceRange config = do
  startIdx <- normalizeBound 0 startVal
  endIdx <- normalizeBound lenNow endVal
  let clampedStart = clamp startIdx
      clampedEnd = clamp endIdx
  Right
    SliceRangeResult
      { sliceRangeResultStart = clampedStart,
        sliceRangeResultEnd = max clampedStart clampedEnd
      }
  where
    pos = sliceRangePos config
    lenNow = sliceRangeLength config
    startVal = sliceRangeStartVal config
    endVal = sliceRangeEndVal config

    clamp idx = max 0 (min lenNow idx)

    normalizeBound defaultVal value =
      case value of
        NoneValue -> Right defaultVal
        IntValue {intValue = idx} ->
          let adjusted = if idx < 0 then idx + fromIntegral lenNow else idx
           in Right (fromIntegral adjusted)
        _ -> Left ("Type error: slice expects int or None bounds at " ++ showPos pos)
