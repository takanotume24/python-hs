module PythonHS.VM.CallStringBuiltin (callStringBuiltin) where

import Data.Char (isSpace, toLower, toUpper)
import Data.List (intercalate, isPrefixOf, isSuffixOf, stripPrefix, tails)
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
    "startswith" -> Just (evalAffix isPrefixOf "startswith" args)
    "endswith" -> Just (evalAffix isSuffixOf "endswith" args)
    "find" -> Just (evalFind args)
    "count" -> Just (evalCount args)
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

    evalAffix matches fname values =
      evalStringPair fname values (\text affix -> boolValue (matches affix text))

    boolValue condition = IntValue {intValue = if condition then 1 else 0}

    evalFind values =
      evalStringPair "find" values (\text needle -> IntValue {intValue = fromIntegral (indexOf needle text)})

    evalCount values =
      evalStringPair "count" values (\text needle -> IntValue {intValue = fromIntegral (countOccurrences needle text)})

    evalStringPair fname values onMatch =
      case values of
        [StringValue {stringValue = firstText}, StringValue {stringValue = secondText}] ->
          Right (onMatch firstText secondText)
        [_, StringValue {}] -> Left (receiverError fname)
        [StringValue {}, _] -> Left (argumentsError fname)
        [StringValue {}] -> Left (countMismatch fname)
        [_, _] -> Left (receiverError fname)
        [_] -> Left (receiverError fname)
        _ -> Left (countMismatch fname)
      where
        receiverError errorName = "Type error: " ++ errorName ++ " expects string at " ++ showPos pos
        argumentsError errorName = "Type error: " ++ errorName ++ " expects string arguments at " ++ showPos pos

    indexOf :: String -> String -> Int
    indexOf needle text
      | null needle = 0
      | otherwise = go 0 text
      where
        go index remaining
          | needle `isPrefixOf` remaining = index
          | otherwise =
              case remaining of
                [] -> -1
                (_ : restCharacters) -> go (index + 1) restCharacters

    countOccurrences :: String -> String -> Int
    countOccurrences needle text
      | null needle = length text + 1
      | otherwise = go 0 text
      where
        go total remaining =
          case stripPrefix needle remaining of
            Just after -> go (total + 1) after
            Nothing ->
              case remaining of
                [] -> total
                (_ : restCharacters) -> go total restCharacters

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
