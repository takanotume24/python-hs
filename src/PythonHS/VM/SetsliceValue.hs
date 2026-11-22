module PythonHS.VM.SetsliceValue (setsliceValue) where

import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.SetsliceValueConfig (SetsliceValueConfig (..))
import PythonHS.VM.SliceRange (sliceRange)
import PythonHS.VM.SliceRangeConfig (SliceRangeConfig (..))
import PythonHS.VM.SliceRangeResult (SliceRangeResult (..))

setsliceValue :: SetsliceValueConfig -> Either String Value
setsliceValue config =
  case seqValue of
    ListValue {listValueItems = vals} -> do
      range <- sliceRange SliceRangeConfig {sliceRangePos = pos, sliceRangeLength = length vals, sliceRangeStartVal = startVal, sliceRangeEndVal = endVal}
      newItems <- iterableItems newItemsValue
      let start = sliceRangeResultStart range
          end = sliceRangeResultEnd range
      Right (ListValue {listValueItems = take start vals ++ newItems ++ drop end vals})
    _ -> Left ("Type error: slice assignment expects list as first argument at " ++ showPos pos)
  where
    pos = setsliceValuePos config
    seqValue = setsliceValueSeqValue config
    startVal = setsliceValueStartVal config
    endVal = setsliceValueEndVal config
    newItemsValue = setsliceValueNewItems config

    iterableItems value =
      case value of
        ListValue {listValueItems = items} -> Right items
        TupleValue {tupleValueItems = items} -> Right items
        _ -> Left ("Type error: can only assign an iterable to a slice at " ++ showPos pos)
