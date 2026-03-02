module PythonHS.VM.GetitemValue (getitemValue) where

import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (IntValue, ListValue, StringValue, TupleValue), Value)
import PythonHS.Lexer.Position (Position)

getitemValue :: Position -> Value -> Value -> Either String Value
getitemValue pos seqValue indexValue =
  case (seqValue, indexValue) of
    (ListValue vals, IntValue idx) -> getAtValues vals idx
    (TupleValue vals, IntValue idx) -> getAtValues vals idx
    (StringValue s, IntValue idx) -> getAtString s idx
    (_, IntValue _) -> Left ("Type error: getitem expects list, tuple, or string as first argument at " ++ showPos pos)
    _ -> Left ("Type error: getitem expects int index at " ++ showPos pos)
  where
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
        Just i -> Right (StringValue [s !! i])
        Nothing -> Left ("Value error: string index out of range at " ++ showPos pos)
