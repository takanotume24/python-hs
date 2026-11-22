module PythonHS.VM.DelitemValue (delitemValue) where

import PythonHS.Evaluator.DictPair (DictPair (..))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.DelitemValueConfig (DelitemValueConfig (..))

delitemValue :: DelitemValueConfig -> Either String Value
delitemValue config =
  case seqValue of
    ListValue {listValueItems = vals}
      | IntValue {intValue = idx} <- indexValue -> deleteAtValues vals idx
    DictValue {dictValuePairs = pairs} -> deleteAtDict pairs
    _
      | IntValue {} <- indexValue ->
          Left ("Type error: delitem expects list or dict as first argument at " ++ showPos pos)
    _ -> Left ("Type error: delitem expects int index at " ++ showPos pos)
  where
    pos = delitemValuePos config
    seqValue = delitemValueSeqValue config
    indexValue = delitemValueIndexValue config

    normalizeIndex lenNow idx =
      let adjusted = if idx < 0 then idx + fromIntegral lenNow else idx
       in if adjusted < 0 || adjusted >= fromIntegral lenNow
            then Nothing
            else Just (fromIntegral adjusted)

    deleteAtValues values idx =
      case normalizeIndex (length values) idx of
        Just i -> Right (ListValue {listValueItems = take i values ++ drop (i + 1) values})
        Nothing -> Left ("Value error: tuple/list index out of range at " ++ showPos pos)

    deleteAtDict pairs =
      case removePair pairs of
        Just remaining -> Right (DictValue {dictValuePairs = remaining})
        Nothing -> Left ("Key not found in delitem at " ++ showPos pos)

    removePair pairs =
      case pairs of
        [] -> Nothing
        pair : restPairs ->
          if dictPairKey pair == indexValue
            then Just restPairs
            else fmap (pair :) (removePair restPairs)
