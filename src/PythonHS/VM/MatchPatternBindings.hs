module PythonHS.VM.MatchPatternBindings (matchPatternBindings) where

import PythonHS.AST.Expr (Expr (DictExpr, FloatExpr, IntegerExpr, ListExpr, NoneExpr, StringExpr, TupleExpr))
import PythonHS.AST.Pattern (Pattern (AsPattern, CapturePattern, MappingPattern, OrPattern, SequencePattern, ValuePattern, WildcardPattern))
import PythonHS.Evaluator.Value (Value (DictValue, FloatValue, IntValue, ListValue, NoneValue, StringValue, TupleValue))

matchPatternBindings :: Pattern -> Value -> Maybe [(String, Value)]
matchPatternBindings patternValue subjectValue =
  case patternValue of
    WildcardPattern _ -> Just []
    CapturePattern name _ -> Just [(name, subjectValue)]
    AsPattern innerPattern aliasName _ -> do
      innerBindings <- matchPatternBindings innerPattern subjectValue
      Just (innerBindings ++ [(aliasName, subjectValue)])
    ValuePattern expr _ ->
      case exprToValue expr of
        Just expected -> if expected == subjectValue then Just [] else Nothing
        Nothing -> Nothing
    OrPattern patterns _ -> firstMatch patterns
    SequencePattern items maybeRest _ ->
      case subjectValue of
        ListValue values ->
          matchSequenceValue items maybeRest values
        TupleValue values ->
          matchSequenceValue items maybeRest values
        _ -> Nothing
    MappingPattern pairs _ ->
      case subjectValue of
        DictValue entries -> matchMappingPairs pairs entries []
        _ -> Nothing
  where
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
               in Just (prefixBindings ++ [(restName, ListValue restValues)])
    firstMatch [] = Nothing
    firstMatch (current : rest) =
      case matchPatternBindings current subjectValue of
        Just binds -> Just binds
        Nothing -> firstMatch rest

    matchSequence [] _ acc = Just acc
    matchSequence (nextPattern : restPatterns) (nextValue : restValues) acc = do
      nextBindings <- matchPatternBindings nextPattern nextValue
      matchSequence restPatterns restValues (acc ++ nextBindings)
    matchSequence _ _ _ = Nothing

    matchMappingPairs [] _ acc = Just acc
    matchMappingPairs ((keyExpr, valuePattern) : restPairs) entries acc = do
      keyValue <- exprToValue keyExpr
      subjectValueAtKey <- lookupKey keyValue entries
      newBindings <- matchPatternBindings valuePattern subjectValueAtKey
      matchMappingPairs restPairs entries (acc ++ newBindings)

    lookupKey _ [] = Nothing
    lookupKey key ((k, v) : rest)
      | key == k = Just v
      | otherwise = lookupKey key rest

    exprToValue (IntegerExpr n _) = Just (IntValue n)
    exprToValue (FloatExpr n _) = Just (FloatValue n)
    exprToValue (StringExpr s _) = Just (StringValue s)
    exprToValue (NoneExpr _) = Just NoneValue
    exprToValue (ListExpr exprs _) = fmap ListValue (mapExprs exprs)
    exprToValue (TupleExpr exprs _) = fmap TupleValue (mapExprs exprs)
    exprToValue (DictExpr pairs _) = fmap DictValue (mapPairs pairs)
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
