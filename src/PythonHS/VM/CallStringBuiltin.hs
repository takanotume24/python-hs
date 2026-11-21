module PythonHS.VM.CallStringBuiltin (callStringBuiltin) where

import Data.Char (isSpace, toLower, toUpper)
import Data.List (intercalate, isPrefixOf, stripPrefix, tails)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.CallStringBuiltinConfig (CallStringBuiltinConfig (..))
import PythonHS.VM.StringItems (stringItems)

callStringBuiltin :: CallStringBuiltinConfig -> Maybe (Either String Value)
callStringBuiltin config =
  case name of
    "split" -> Just (evalSplit args)
    "join" -> Just (evalJoin args)
    "lower" -> Just (evalCase toLower "lower" args)
    "upper" -> Just (evalCase toUpper "upper" args)
    "strip" -> Just (evalStrip args)
    "replace" -> Just (evalReplace args)
    "count" -> evalListCount args
    _ -> Nothing
  where
    name = callStringBuiltinName config
    args = callStringBuiltinArgs config
    pos = callStringBuiltinPos config

    evalSplit values =
      case values of
        [StringValue {stringValue = text}] -> Right (stringList (words text))
        [StringValue {stringValue = text}, StringValue {stringValue = separator}]
          | null separator -> Left ("Value error: empty separator at " ++ showPos pos)
          | otherwise -> Right (stringList (splitOn separator text))
        [StringValue {}, _] -> Left ("Type error: split expects string separator at " ++ showPos pos)
        [_] -> Left ("Type error: split expects string at " ++ showPos pos)
        _ -> Left (countMismatch "split")

    evalJoin values =
      case values of
        [StringValue {stringValue = separator}, ListValue {listValueItems = items}] -> joinItems separator items
        [StringValue {stringValue = separator}, TupleValue {tupleValueItems = items}] -> joinItems separator items
        [StringValue {}, _] -> Left ("Type error: join expects list or tuple as second argument at " ++ showPos pos)
        [_, _] -> Left ("Type error: join expects string as first argument at " ++ showPos pos)
        _ -> Left (countMismatch "join")

    joinItems separator items =
      case stringItems items of
        Nothing -> Left ("Type error: join expects list of string at " ++ showPos pos)
        Just texts -> Right (StringValue {stringValue = intercalate separator texts})

    evalCase convert fname values =
      case values of
        [StringValue {stringValue = text}] -> Right (StringValue {stringValue = map convert text})
        [_] -> Left ("Type error: " ++ fname ++ " expects string at " ++ showPos pos)
        _ -> Left (countMismatch fname)

    evalStrip values =
      case values of
        [StringValue {stringValue = text}] -> Right (StringValue {stringValue = trimSpace text})
        [StringValue {stringValue = text}, StringValue {stringValue = chars}] -> Right (StringValue {stringValue = trimChars chars text})
        [StringValue {}, _] -> Left ("Type error: strip expects string chars at " ++ showPos pos)
        [_] -> Left ("Type error: strip expects string at " ++ showPos pos)
        _ -> Left (countMismatch "strip")

    trimSpace text = reverse (dropWhile isSpace (reverse (dropWhile isSpace text)))

    trimChars chars text =
      reverse (dropWhile (`elem` chars) (reverse (dropWhile (`elem` chars) text)))

    evalReplace values =
      case values of
        [StringValue {stringValue = text}, StringValue {stringValue = oldValue}, StringValue {stringValue = newValue}] ->
          Right (StringValue {stringValue = replaceAll oldValue newValue text})
        [_, _, _] -> Left ("Type error: replace expects string arguments at " ++ showPos pos)
        _ -> Left (countMismatch "replace")

    evalListCount values =
      case values of
        [ListValue {listValueItems = items}, targetValue] ->
          Just (Right (IntValue {intValue = fromIntegral (length (filter (== targetValue) items))}))
        (ListValue {} : _) -> Just (Left (countMismatch "count"))
        _ -> Nothing

    replaceAll oldValue newValue text
      | null oldValue = concatMap (\character -> newValue ++ [character]) text ++ newValue
      | otherwise = go text
      where
        go remaining =
          case stripPrefix oldValue remaining of
            Just after -> newValue ++ go after
            Nothing ->
              case remaining of
                [] -> []
                (character : restCharacters) -> character : go restCharacters

    splitOn separator text = go text
      where
        go remaining =
          case break (separator `isPrefixOf`) (tails remaining) of
            (_, []) -> [remaining]
            (matched, _ : _) ->
              let partLength = length matched
               in take partLength remaining : go (drop (partLength + length separator) remaining)

    stringList texts =
      ListValue {listValueItems = map (\text -> StringValue {stringValue = text}) texts}

    countMismatch fname = "Argument count mismatch when calling " ++ fname ++ " at " ++ showPos pos
