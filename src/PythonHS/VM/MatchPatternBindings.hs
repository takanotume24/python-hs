module PythonHS.VM.MatchPatternBindings (matchPatternBindings) where

import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.Pattern (Pattern (..))
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.MatchPatternBindingsConfig (MatchPatternBindingsConfig (..))

matchPatternBindings :: MatchPatternBindingsConfig -> Maybe [(String, Value)]
matchPatternBindings config = case patternValue of
  WildcardPattern {} -> Just []
  CapturePattern {capturePatternName = name} -> Just [(name, subjectValue)]
  AsPattern {asPatternInner = innerPattern, asPatternAlias = aliasName} -> do
    innerBindings <- matchPatternBindings MatchPatternBindingsConfig {matchPatternBindingsPattern = innerPattern, matchPatternBindingsSubject = subjectValue}
    Just (innerBindings ++ [(aliasName, subjectValue)])
  ValuePattern {valuePatternExpr = expr} ->
    case exprToValue expr of
      Just expected -> if expected == subjectValue then Just [] else Nothing
      Nothing -> Nothing
  OrPattern {orPatternItems = patterns} -> firstMatch patterns
  SequencePattern {sequencePatternItems = items, sequencePatternRest = maybeRest} ->
    case subjectValue of
      ListValue {listValueItems = values} ->
        matchSequenceValue items maybeRest values
      TupleValue {tupleValueItems = values} ->
        matchSequenceValue items maybeRest values
      _ -> Nothing
  MappingPattern {mappingPatternPairs = pairs, mappingPatternRest = maybeRestCapture} ->
    case subjectValue of
      DictValue {dictValuePairs = entries} ->
        case matchMappingPairs pairs entries [] [] of
          Just (bindings, matchedKeys) ->
            case maybeRestCapture of
              Nothing -> Just bindings
              Just restName ->
                let restEntries = filter (\(k, _) -> notElem k matchedKeys) entries
                 in Just (bindings ++ [(restName, DictValue {dictValuePairs = restEntries})])
          Nothing -> Nothing
      _ -> Nothing
  where
    patternValue = matchPatternBindingsPattern config
    subjectValue = matchPatternBindingsSubject config
    matchSequenceValue items maybeRest values =
      if length values < length items
        then Nothing
        else do
          prefixBindings <- matchSequence items values []
          case maybeRest of
            Nothing ->
              if length values == length items
                then Just prefixBindings
                else Nothing
            Just restName ->
              let restValues = drop (length items) values
               in Just (prefixBindings ++ [(restName, ListValue {listValueItems = restValues})])
    firstMatch [] = Nothing
    firstMatch (current : rest) =
      case matchPatternBindings MatchPatternBindingsConfig {matchPatternBindingsPattern = current, matchPatternBindingsSubject = subjectValue} of
        Just binds -> Just binds
        Nothing -> firstMatch rest

    matchSequence [] _ acc = Just acc
    matchSequence (nextPattern : restPatterns) (nextValue : restValues) acc = do
      nextBindings <- matchPatternBindings MatchPatternBindingsConfig {matchPatternBindingsPattern = nextPattern, matchPatternBindingsSubject = nextValue}
      matchSequence restPatterns restValues (acc ++ nextBindings)
    matchSequence _ _ _ = Nothing

    matchMappingPairs [] _ acc matchedKeys = Just (acc, matchedKeys)
    matchMappingPairs ((keyExpr, valuePattern) : restPairs) entries acc matchedKeys = do
      keyValue <- exprToValue keyExpr
      subjectValueAtKey <- lookupKey keyValue entries
      newBindings <- matchPatternBindings MatchPatternBindingsConfig {matchPatternBindingsPattern = valuePattern, matchPatternBindingsSubject = subjectValueAtKey}
      matchMappingPairs restPairs entries (acc ++ newBindings) (matchedKeys ++ [keyValue])

    lookupKey _ [] = Nothing
    lookupKey key ((k, v) : rest)
      | key == k = Just v
      | otherwise = lookupKey key rest

    exprToValue IntegerExpr {integerExprValue = n} = Just (IntValue {intValue = n})
    exprToValue FloatExpr {floatExprValue = n} = Just (FloatValue {floatValue = n})
    exprToValue StringExpr {stringExprValue = s} = Just (StringValue {stringValue = s})
    exprToValue NoneExpr {} = Just NoneValue
    exprToValue ListExpr {listExprItems = exprs} = fmap ListValue (mapExprs exprs)
    exprToValue TupleExpr {tupleExprItems = exprs} = fmap TupleValue (mapExprs exprs)
    exprToValue DictExpr {dictExprEntries = pairs} = fmap DictValue (mapPairs pairs)
    exprToValue _ = Nothing

    mapExprs [] = Just []
    mapExprs (expr : rest) = do
      firstValue <- exprToValue expr
      restValues <- mapExprs rest
      Just (firstValue : restValues)

    mapPairs [] = Just []
    mapPairs ((keyExpr, valueExpr) : rest) = do
      keyValue <- exprToValue keyExpr
      valueValue <- exprToValue valueExpr
      restPairs <- mapPairs rest
      Just ((keyValue, valueValue) : restPairs)
