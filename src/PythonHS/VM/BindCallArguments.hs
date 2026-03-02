module PythonHS.VM.BindCallArguments (bindCallArguments) where

import qualified Data.Map.Strict as Map
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (DictValue, ListValue, StringValue), Value)
import PythonHS.Lexer.Position (Position)

bindCallArguments :: String -> Position -> [String] -> [Value] -> [(Maybe String, Position)] -> Either String (Map.Map String Value)
bindCallArguments fname callPos params argValues argKinds = do
  (positionalVals, keywordVals, kwPosByName, kwOrder) <- collectArgs False [] Map.empty Map.empty [] (zip argValues argKinds)
  boundByName <- bindByName keywordVals kwPosByName kwOrder
  bindPositional positionalVals boundByName kwPosByName kwOrder keywordVals
  where
    plainParams = filter isPlainParam params
    varArgName = findVarArg params
    kwArgName = findKwArg params

    collectArgs seenKeywordArg positionalVals keywordVals kwPosByName kwOrder pairs =
      case pairs of
        [] -> Right (positionalVals, keywordVals, kwPosByName, kwOrder)
        (argValue, (argKind, argPos)) : restPairs ->
          case argKind of
            Nothing ->
              if seenKeywordArg
                then Left ("Argument count mismatch when calling " ++ fname ++ " at " ++ showPos callPos)
                else collectArgs False (positionalVals ++ [argValue]) keywordVals kwPosByName kwOrder restPairs
            Just argName ->
              case Map.lookup argName keywordVals of
                Just _ -> Left ("Argument error: duplicate keyword argument " ++ argName ++ " at " ++ showPos argPos)
                Nothing ->
                  collectArgs
                    True
                    positionalVals
                    (Map.insert argName argValue keywordVals)
                    (Map.insert argName argPos kwPosByName)
                    (kwOrder ++ [argName])
                    restPairs

    bindByName keywordVals kwPosByName kwOrder =
      case firstUnexpected kwOrder of
        Just unexpectedName ->
          case Map.lookup unexpectedName kwPosByName of
            Just argPos -> Left ("Argument error: unexpected keyword argument " ++ unexpectedName ++ " at " ++ showPos argPos)
            Nothing -> Left ("Argument count mismatch when calling " ++ fname ++ " at " ++ showPos callPos)
        Nothing ->
          case firstUnexpected (Map.keys keywordVals) of
            Just unexpectedName ->
              case Map.lookup unexpectedName kwPosByName of
                Just argPos -> Left ("Argument error: unexpected keyword argument " ++ unexpectedName ++ " at " ++ showPos argPos)
                Nothing -> Left ("Argument count mismatch when calling " ++ fname ++ " at " ++ showPos callPos)
            Nothing -> Right keywordVals

    bindPositional positionalVals bound kwPosByName kwOrder keywordVals =
      case firstCollisionName of
        Just paramName ->
          case Map.lookup paramName kwPosByName of
            Just argPos -> Left ("Argument error: multiple values for parameter " ++ paramName ++ " at " ++ showPos argPos)
            Nothing -> Left ("Argument count mismatch when calling " ++ fname ++ " at " ++ showPos callPos)
        Nothing ->
          if length positionalVals > length plainParams && varArgName == Nothing
            then Left ("Argument count mismatch when calling " ++ fname ++ " at " ++ showPos callPos)
            else
              let fixedBound = foldl insertPositional bound (zip positionalParamNames positionalVals)
                  withVarArgs =
                    case varArgName of
                      Just name -> Map.insert name (ListValue (drop (length plainParams) positionalVals)) fixedBound
                      Nothing -> fixedBound
                  withKwArgs =
                    case kwArgName of
                      Just name -> Map.insert name (DictValue (fmap (\(k, v) -> (StringValue k, v)) (extraKeywordPairs kwOrder keywordVals))) withVarArgs
                      Nothing -> withVarArgs
               in Right withKwArgs
      where
        positionalParamNames = take (min (length positionalVals) (length plainParams)) plainParams
        firstCollisionName = firstMatch kwOrder positionalParamNames

        insertPositional acc (paramName, value) =
          case Map.lookup paramName acc of
            Just _ -> acc
            Nothing -> Map.insert paramName value acc

    firstUnexpected names = firstNotInParams names

    firstNotInParams names =
      case names of
        [] -> Nothing
        name : restNames ->
          if name `elem` plainParams || kwArgName /= Nothing
            then firstNotInParams restNames
            else Just name

    firstMatch names candidates =
      case names of
        [] -> Nothing
        name : restNames ->
          if name `elem` candidates
            then Just name
            else firstMatch restNames candidates

    isPlainParam name = not (isVarParam name) && not (isKwParam name)

    isVarParam ('*' : c : _) = c /= '*'
    isVarParam _ = False

    isKwParam ('*' : '*' : _) = True
    isKwParam _ = False

    stripPrefixStars ('*' : '*' : rest) = rest
    stripPrefixStars ('*' : rest) = rest
    stripPrefixStars name = name

    findVarArg names =
      case filter isVarParam names of
        [] -> Nothing
        name : _ -> Just (stripPrefixStars name)

    findKwArg names =
      case filter isKwParam names of
        [] -> Nothing
        name : _ -> Just (stripPrefixStars name)

    extraKeywordPairs names keywordVals =
      case names of
        [] -> []
        name : restNames ->
          if name `elem` plainParams
            then extraKeywordPairs restNames keywordVals
            else
              case Map.lookup name keywordVals of
                Just value -> (name, value) : extraKeywordPairs restNames keywordVals
                Nothing -> extraKeywordPairs restNames keywordVals
