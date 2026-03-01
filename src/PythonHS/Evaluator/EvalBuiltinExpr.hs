module PythonHS.Evaluator.EvalBuiltinExpr (evalBuiltinExpr) where

import Data.List (sortOn)
import PythonHS.AST.Expr (Expr)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Evaluator.RangeValues (rangeValues)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (DictValue, FloatValue, IntValue, ListValue, NoneValue, StringValue))
import PythonHS.Lexer.Position (Position)

evalBuiltinExpr :: (Env -> FuncEnv -> Expr -> Either String (Value, [String], Env)) -> Env -> FuncEnv -> String -> [Expr] -> Position -> Maybe (Either String (Value, [String], Env))
evalBuiltinExpr evalExprFn env fenv fname args pos =
  case fname of
    "len" -> Just $ do
      (argVals, argOuts, envAfterArgs) <- evalArgs env fenv args
      case argVals of
        [StringValue s] -> Right (IntValue (fromIntegral (length s)), argOuts, envAfterArgs)
        [ListValue vals] -> Right (IntValue (fromIntegral (length vals)), argOuts, envAfterArgs)
        [_] -> Left $ "Type error: len expects string or list at " ++ showPos pos
        _ -> Left $ "Argument count mismatch when calling len at " ++ showPos pos
    "bool" -> Just $ do
      (argVals, argOuts, envAfterArgs) <- evalArgs env fenv args
      case argVals of
        [IntValue n] -> Right (IntValue (if n == 0 then 0 else 1), argOuts, envAfterArgs)
        [FloatValue n] -> Right (IntValue (if n == 0 then 0 else 1), argOuts, envAfterArgs)
        [NoneValue] -> Right (IntValue 0, argOuts, envAfterArgs)
        [StringValue s] -> Right (IntValue (if null s then 0 else 1), argOuts, envAfterArgs)
        [ListValue vals] -> Right (IntValue (if null vals then 0 else 1), argOuts, envAfterArgs)
        [DictValue pairs] -> Right (IntValue (if null pairs then 0 else 1), argOuts, envAfterArgs)
        _ -> Left $ "Argument count mismatch when calling bool at " ++ showPos pos
    "range" -> Just $ do
      (argVals, argOuts, envAfterArgs) <- evalArgs env fenv args
      case argVals of
        [IntValue n] -> Right (ListValue (map IntValue (rangeOne n)), argOuts, envAfterArgs)
        [IntValue start, IntValue stop] -> Right (ListValue (map IntValue (rangeValues start stop 1)), argOuts, envAfterArgs)
        [IntValue start, IntValue stop, IntValue step] ->
          if step == 0
            then Left $ "Value error: range step must not be zero at " ++ showPos pos
            else Right (ListValue (map IntValue (rangeValues start stop step)), argOuts, envAfterArgs)
        [_] -> Left $ "Type error: range expects int at " ++ showPos pos
        [_, _] -> Left $ "Type error: range expects int arguments at " ++ showPos pos
        [_, _, _] -> Left $ "Type error: range expects int arguments at " ++ showPos pos
        _ -> Left $ "Argument count mismatch when calling range at " ++ showPos pos
    "append" -> Just $ do
      (argVals, argOuts, envAfterArgs) <- evalArgs env fenv args
      case argVals of
        [ListValue vals, value] -> Right (ListValue (vals ++ [value]), argOuts, envAfterArgs)
        [_, _] -> Left $ "Type error: append expects list as first argument at " ++ showPos pos
        _ -> Left $ "Argument count mismatch when calling append at " ++ showPos pos
    "sort" -> Just $ do
      (argVals, argOuts, envAfterArgs) <- evalArgs env fenv args
      case argVals of
        [ListValue vals] ->
          case numberPairs vals of
            Just pairs -> Right (ListValue (map snd (sortOn fst pairs)), argOuts, envAfterArgs)
            Nothing -> Left $ "Type error: sort expects list of number at " ++ showPos pos
        [_] -> Left $ "Type error: sort expects list as first argument at " ++ showPos pos
        _ -> Left $ "Argument count mismatch when calling sort at " ++ showPos pos
    "reverse" -> Just $ do
      (argVals, argOuts, envAfterArgs) <- evalArgs env fenv args
      case argVals of
        [ListValue vals] -> Right (ListValue (reverse vals), argOuts, envAfterArgs)
        [_] -> Left $ "Type error: reverse expects list as first argument at " ++ showPos pos
        _ -> Left $ "Argument count mismatch when calling reverse at " ++ showPos pos
    "remove" -> Just $ do
      (argVals, argOuts, envAfterArgs) <- evalArgs env fenv args
      case argVals of
        [ListValue vals, target] ->
          case removeFirstValue vals target of
            Just newVals -> Right (ListValue newVals, argOuts, envAfterArgs)
            Nothing -> Left $ "Value error: remove value not found at " ++ showPos pos
        [_, _] -> Left $ "Type error: remove expects list as first argument at " ++ showPos pos
        _ -> Left $ "Argument count mismatch when calling remove at " ++ showPos pos
    "insert" -> Just $ do
      (argVals, argOuts, envAfterArgs) <- evalArgs env fenv args
      case argVals of
        [ListValue vals, IntValue index, value] -> Right (ListValue (insertAtIndex vals index value), argOuts, envAfterArgs)
        [ListValue _, _, _] -> Left $ "Type error: insert expects int index at " ++ showPos pos
        [_, _, _] -> Left $ "Type error: insert expects list as first argument at " ++ showPos pos
        _ -> Left $ "Argument count mismatch when calling insert at " ++ showPos pos
    "pop" -> Just $ do
      (argVals, argOuts, envAfterArgs) <- evalArgs env fenv args
      case argVals of
        [ListValue []] -> Left $ "Value error: pop from empty list at " ++ showPos pos
        [ListValue vals] -> Right (last vals, argOuts, envAfterArgs)
        [DictValue pairs, key] ->
          case lookupDictValue pairs key of
            Just value -> Right (value, argOuts, envAfterArgs)
            Nothing -> Left $ "Key not found in pop at " ++ showPos pos
        [DictValue pairs, key, defaultValue] ->
          case lookupDictValue pairs key of
            Just value -> Right (value, argOuts, envAfterArgs)
            Nothing -> Right (defaultValue, argOuts, envAfterArgs)
        [_] -> Left $ "Type error: pop expects list at " ++ showPos pos
        [ListValue _, _] -> Left $ "Argument count mismatch when calling pop at " ++ showPos pos
        [ListValue _, _, _] -> Left $ "Argument count mismatch when calling pop at " ++ showPos pos
        [_, _] -> Left $ "Type error: pop expects dict as first argument at " ++ showPos pos
        [_, _, _] -> Left $ "Type error: pop expects dict as first argument at " ++ showPos pos
        _ -> Left $ "Argument count mismatch when calling pop at " ++ showPos pos
    "clear" -> Just $ do
      (argVals, argOuts, envAfterArgs) <- evalArgs env fenv args
      case argVals of
        [ListValue _] -> Right (ListValue [], argOuts, envAfterArgs)
        [DictValue _] -> Right (DictValue [], argOuts, envAfterArgs)
        [_] -> Left $ "Type error: clear expects list or dict at " ++ showPos pos
        _ -> Left $ "Argument count mismatch when calling clear at " ++ showPos pos
    "keys" -> Just $ do
      (argVals, argOuts, envAfterArgs) <- evalArgs env fenv args
      case argVals of
        [DictValue pairs] -> Right (ListValue (map fst pairs), argOuts, envAfterArgs)
        [_] -> Left $ "Type error: keys expects dict at " ++ showPos pos
        _ -> Left $ "Argument count mismatch when calling keys at " ++ showPos pos
    "get" -> Just $ do
      (argVals, argOuts, envAfterArgs) <- evalArgs env fenv args
      case argVals of
        [DictValue pairs, key] ->
          case lookupDictValue pairs key of
            Just value -> Right (value, argOuts, envAfterArgs)
            Nothing -> Left $ "Key not found in get at " ++ showPos pos
        [DictValue pairs, key, defaultValue] ->
          case lookupDictValue pairs key of
            Just value -> Right (value, argOuts, envAfterArgs)
            Nothing -> Right (defaultValue, argOuts, envAfterArgs)
        [_, _] -> Left $ "Type error: get expects dict as first argument at " ++ showPos pos
        [_, _, _] -> Left $ "Type error: get expects dict as first argument at " ++ showPos pos
        _ -> Left $ "Argument count mismatch when calling get at " ++ showPos pos
    "update" -> Just $ do
      (argVals, argOuts, envAfterArgs) <- evalArgs env fenv args
      case argVals of
        [DictValue pairs, DictValue otherPairs] -> Right (DictValue (mergeDictValues pairs otherPairs), argOuts, envAfterArgs)
        [DictValue _, _] -> Left $ "Type error: update expects dict as second argument at " ++ showPos pos
        [DictValue pairs, key, value] -> Right (DictValue (updateDictValue pairs key value), argOuts, envAfterArgs)
        [_, _, _] -> Left $ "Type error: update expects dict as first argument at " ++ showPos pos
        [_, _] -> Left $ "Type error: update expects dict as first argument at " ++ showPos pos
        _ -> Left $ "Argument count mismatch when calling update at " ++ showPos pos
    "setdefault" -> Just $ do
      (argVals, argOuts, envAfterArgs) <- evalArgs env fenv args
      case argVals of
        [DictValue pairs, key] -> Right (DictValue (setDefaultDictValue pairs key NoneValue), argOuts, envAfterArgs)
        [DictValue pairs, key, defaultValue] -> Right (DictValue (setDefaultDictValue pairs key defaultValue), argOuts, envAfterArgs)
        [_, _] -> Left $ "Type error: setdefault expects dict as first argument at " ++ showPos pos
        [_, _, _] -> Left $ "Type error: setdefault expects dict as first argument at " ++ showPos pos
        _ -> Left $ "Argument count mismatch when calling setdefault at " ++ showPos pos
    "values" -> Just $ do
      (argVals, argOuts, envAfterArgs) <- evalArgs env fenv args
      case argVals of
        [DictValue pairs] -> Right (ListValue (map snd pairs), argOuts, envAfterArgs)
        [_] -> Left $ "Type error: values expects dict at " ++ showPos pos
        _ -> Left $ "Argument count mismatch when calling values at " ++ showPos pos
    "items" -> Just $ do
      (argVals, argOuts, envAfterArgs) <- evalArgs env fenv args
      case argVals of
        [DictValue pairs] -> Right (ListValue (map pairToList pairs), argOuts, envAfterArgs)
        [_] -> Left $ "Type error: items expects dict at " ++ showPos pos
        _ -> Left $ "Argument count mismatch when calling items at " ++ showPos pos
    _ -> Nothing
  where
    evalArgs currentEnv currentFenv = foldl go (Right ([], [], currentEnv))
      where
        go acc expr = do
          (vals, outs, envNow) <- acc
          (value, exprOuts, envNext) <- evalExprFn envNow currentFenv expr
          Right (vals ++ [value], outs ++ exprOuts, envNext)
    lookupDictValue [] _ = Nothing
    lookupDictValue ((k, v) : restPairs) target
      | k == target = Just v
      | otherwise = lookupDictValue restPairs target
    updateDictValue [] key value = [(key, value)]
    updateDictValue ((k, v) : restPairs) key value
      | k == key = (k, value) : restPairs
      | otherwise = (k, v) : updateDictValue restPairs key value
    mergeDictValues pairs [] = pairs
    mergeDictValues pairs ((key, value) : restPairs) =
      mergeDictValues (updateDictValue pairs key value) restPairs
    setDefaultDictValue pairs key defaultValue =
      case lookupDictValue pairs key of
        Just _ -> pairs
        Nothing -> pairs ++ [(key, defaultValue)]
    removeFirstValue [] _ = Nothing
    removeFirstValue (v : restVals) target
      | v == target = Just restVals
      | otherwise = fmap (v :) (removeFirstValue restVals target)
    numberPairs [] = Just []
    numberPairs (IntValue n : restVals) = fmap ((fromIntegral n, IntValue n) :) (numberPairs restVals)
    numberPairs (FloatValue n : restVals) = fmap ((n, FloatValue n) :) (numberPairs restVals)
    numberPairs (_ : _) = Nothing

    insertAtIndex values index value =
      let maxIndex = fromIntegral (length values)
          clampedIndex = max 0 (min index maxIndex)
          splitIndex = fromIntegral clampedIndex
          (leftValues, rightValues) = splitAt splitIndex values
       in leftValues ++ (value : rightValues)

    pairToList (k, v) = ListValue [k, v]
    rangeOne n
      | n <= 0 = []
      | otherwise = [0 .. n - 1]
