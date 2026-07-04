module PythonHS.VM.GetitemValue (getitemValue) where

import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.GetitemValueConfig (GetitemValueConfig (..))

getitemValue :: GetitemValueConfig -> Either String Value
getitemValue config = case (seqValue, indexValue) of
  (ListValue {listValueItems = vals}, IntValue {intValue = idx}) -> getAtValues vals idx
  (TupleValue {tupleValueItems = vals}, IntValue {intValue = idx}) -> getAtValues vals idx
  (StringValue {stringValue = s}, IntValue {intValue = idx}) -> getAtString s idx
  (_, IntValue {}) -> Left ("Type error: getitem expects list, tuple, or string as first argument at " ++ showPos pos)
  _ -> Left ("Type error: getitem expects int index at " ++ showPos pos)
  where
    pos = getitemValuePos config
    seqValue = getitemValueSeqValue config
    indexValue = getitemValueIndexValue config
    normalizeIndex lenNow idx =
      let adjusted = if idx < 0 then idx + fromIntegral lenNow else idx
       in if adjusted < 0 || adjusted >= fromIntegral lenNow
            then Nothing
            else Just (fromIntegral adjusted)

    getAtValues values idx =
      case normalizeIndex (length values) idx of
        Just i -> Right (values !! i)
        Nothing -> Left ("Value error: tuple/list index out of range at " ++ showPos pos)

    getAtString s idx =
      case normalizeIndex (length s) idx of
        Just i -> Right (StringValue {stringValue = [s !! i]})
        Nothing -> Left ("Value error: string index out of range at " ++ showPos pos)
