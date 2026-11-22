module PythonHS.VM.DelsliceValue (delsliceValue) where

import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.DelsliceValueConfig (DelsliceValueConfig (..))
import PythonHS.VM.SliceRange (sliceRange)
import PythonHS.VM.SliceRangeConfig (SliceRangeConfig (..))
import PythonHS.VM.SliceRangeResult (SliceRangeResult (..))

delsliceValue :: DelsliceValueConfig -> Either String Value
delsliceValue config =
  case seqValue of
    ListValue {listValueItems = vals} -> do
      range <- sliceRange SliceRangeConfig {sliceRangePos = pos, sliceRangeLength = length vals, sliceRangeStartVal = startVal, sliceRangeEndVal = endVal}
      let start = sliceRangeResultStart range
          end = sliceRangeResultEnd range
      Right (ListValue {listValueItems = take start vals ++ drop end vals})
    _ -> Left ("Type error: del slice expects list as first argument at " ++ showPos pos)
  where
    pos = delsliceValuePos config
    seqValue = delsliceValueSeqValue config
    startVal = delsliceValueStartVal config
    endVal = delsliceValueEndVal config
