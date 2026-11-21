module PythonHS.VM.CallStringSearchBuiltin (callStringSearchBuiltin) where

import Data.List (isPrefixOf, isSuffixOf)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.CallStringBuiltinConfig (CallStringBuiltinConfig (..))

callStringSearchBuiltin :: CallStringBuiltinConfig -> Maybe (Either String Value)
callStringSearchBuiltin config =
  case name of
    "startswith" -> Just (evalAffix isPrefixOf "startswith" args)
    "endswith" -> Just (evalAffix isSuffixOf "endswith" args)
    "find" -> Just (evalSearch False "find" args)
    "rfind" -> Just (evalSearch True "rfind" args)
    "index" -> claimStringArgument (evalIndex "index" False args)
    "rindex" -> Just (evalIndex "rindex" True args)
    "count" -> Just (evalCount args)
    _ -> Nothing
  where
    name = callStringBuiltinName config
    args = callStringBuiltinArgs config
    pos = callStringBuiltinPos config

    claimStringArgument result =
      case args of
        (StringValue {} : _) -> Just result
        _ -> Nothing

    evalAffix matches fname values =
      case rangeArguments fname values of
        Left message -> Left message
        Right (text, affix, maybeStart, maybeEnd) ->
          case rangeSliceMaybe text maybeStart maybeEnd of
            Nothing -> Right (boolValue False)
            Just sliced -> Right (boolValue (matches affix sliced))

    evalSearch backwards fname values =
      case rangeArguments fname values of
        Left message -> Left message
        Right (text, needle, maybeStart, maybeEnd) ->
          let (start, end) = rangeBounds (length text) maybeStart maybeEnd
              found = if backwards then rfindIndex needle text start end else findIndexFrom needle text start end
           in Right (IntValue {intValue = fromIntegral found})

    evalIndex fname backwards values =
      case rangeArguments fname values of
        Left message -> Left message
        Right (text, needle, maybeStart, maybeEnd) ->
          let (start, end) = rangeBounds (length text) maybeStart maybeEnd
              found = if backwards then rfindIndex needle text start end else findIndexFrom needle text start end
           in if found < 0
                then Left ("Value error: substring not found at " ++ showPos pos)
                else Right (IntValue {intValue = fromIntegral found})

    evalCount values =
      case rangeArguments "count" values of
        Left message -> Left message
        Right (text, needle, maybeStart, maybeEnd) ->
          let (start, end) = rangeBounds (length text) maybeStart maybeEnd
           in Right (IntValue {intValue = fromIntegral (countIn needle text start end)})

    rangeArguments fname values =
      case values of
        [StringValue {stringValue = text}, StringValue {stringValue = needle}] ->
          Right (text, needle, Nothing, Nothing)
        [StringValue {stringValue = text}, StringValue {stringValue = needle}, IntValue {intValue = start}] ->
          Right (text, needle, Just start, Nothing)
        [StringValue {stringValue = text}, StringValue {stringValue = needle}, IntValue {intValue = start}, IntValue {intValue = end}] ->
          Right (text, needle, Just start, Just end)
        _ -> Left (argumentMessage fname values)

    argumentMessage fname values =
      case values of
        [StringValue {}] -> countMismatch fname
        [_] -> receiverError fname
        [StringValue {}, StringValue {}] -> countMismatch fname
        [StringValue {}, _] -> argumentsError fname
        [_, _] -> receiverError fname
        [StringValue {}, StringValue {}, _] -> intRangeError fname
        [StringValue {}, _, _] -> argumentsError fname
        [_, _, _] -> receiverError fname
        [StringValue {}, StringValue {}, _, _] -> intRangeError fname
        [StringValue {}, _, _, _] -> argumentsError fname
        [_, _, _, _] -> receiverError fname
        _ -> countMismatch fname

    rangeSliceMaybe text maybeStart maybeEnd =
      let total = length text
          (start, end) = rangeBounds total maybeStart maybeEnd
       in if start > total || end < start
            then Nothing
            else Just (take (end - start) (drop start text))

    rangeBounds total maybeStart maybeEnd =
      (adjustStart total maybeStart, adjustEnd total maybeEnd)

    adjustStart total maybeValue =
      case maybeValue of
        Nothing -> 0
        Just value -> max 0 (if value < 0 then total + fromIntegral value else fromIntegral value)

    adjustEnd total maybeValue =
      case maybeValue of
        Nothing -> total
        Just value -> max 0 (min total (if value < 0 then total + fromIntegral value else fromIntegral value))

    findIndexFrom needle text start end
      | start > end = -1
      | null needle = if start <= length text then start else -1
      | otherwise = go start
      where
        go index
          | index + length needle > end = -1
          | needle `isPrefixOf` drop index text = index
          | otherwise = go (index + 1)

    rfindIndex needle text start end
      | start > end = -1
      | null needle = if start <= length text then min end (length text) else -1
      | otherwise = go (min (end - length needle) (length text - length needle))
      where
        go index
          | index < start = -1
          | needle `isPrefixOf` drop index text = index
          | otherwise = go (index - 1)

    countIn needle text start end
      | start > end = 0
      | null needle = end - start + 1
      | otherwise = go start 0
      where
        go index total
          | index + length needle > end = total
          | needle `isPrefixOf` drop index text = go (index + length needle) (total + 1)
          | otherwise = go (index + 1) total

    boolValue condition = IntValue {intValue = if condition then 1 else 0}

    countMismatch fname = "Argument count mismatch when calling " ++ fname ++ " at " ++ showPos pos

    receiverError fname = "Type error: " ++ fname ++ " expects string at " ++ showPos pos

    argumentsError fname = "Type error: " ++ fname ++ " expects string arguments at " ++ showPos pos

    intRangeError fname = "Type error: " ++ fname ++ " expects int start/end at " ++ showPos pos
