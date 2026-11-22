module PythonHS.VM.SetitemValue (setitemValue) where

import PythonHS.Evaluator.DictPair (DictPair (..))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.SetitemValueConfig (SetitemValueConfig (..))

setitemValue :: SetitemValueConfig -> Either String Value
setitemValue config =
  case seqValue of
    ListValue {listValueItems = vals}
      | IntValue {intValue = idx} <- indexValue -> setAtValues vals idx
    DictValue {dictValuePairs = pairs} -> Right (DictValue {dictValuePairs = upsertPair pairs})
    _
      | IntValue {} <- indexValue ->
          Left ("Type error: setitem expects list or dict as first argument at " ++ showPos pos)
    _ -> Left ("Type error: setitem expects int index at " ++ showPos pos)
  where
    pos = setitemValuePos config
    seqValue = setitemValueSeqValue config
    indexValue = setitemValueIndexValue config
    newValue = setitemValueNewValue config

    normalizeIndex lenNow idx =
      let adjusted = if idx < 0 then idx + fromIntegral lenNow else idx
       in if adjusted < 0 || adjusted >= fromIntegral lenNow
            then Nothing
            else Just (fromIntegral adjusted)

    setAtValues values idx =
      case normalizeIndex (length values) idx of
        Just i -> Right (ListValue {listValueItems = take i values ++ [newValue] ++ drop (i + 1) values})
        Nothing -> Left ("Value error: tuple/list index out of range at " ++ showPos pos)

    upsertPair pairs =
      case pairs of
        [] -> [DictPair {dictPairKey = indexValue, dictPairValue = newValue}]
        pair : restPairs ->
          if dictPairKey pair == indexValue
            then DictPair {dictPairKey = dictPairKey pair, dictPairValue = newValue} : restPairs
            else pair : upsertPair restPairs
