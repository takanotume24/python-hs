module PythonHS.Evaluator.EvalBuiltinExpr (evalBuiltinExpr) where

import Data.List (sortOn)
import PythonHS.AST.Expr (Expr)
import PythonHS.Evaluator.EvalBuiltinExprInput (EvalBuiltinExprInput (..))
import PythonHS.Evaluator.EvalExprResult (EvalExprResult (..))
import PythonHS.Evaluator.InsertAtIndex (insertAtIndex)
import PythonHS.Evaluator.InsertAtIndexConfig (InsertAtIndexConfig (..))
import PythonHS.Evaluator.PairToList (pairToList)
import PythonHS.Evaluator.RangeOne (rangeOne)
import PythonHS.Evaluator.RangeValues (rangeValues)
import PythonHS.Evaluator.RangeValuesConfig (RangeValuesConfig (..))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Lexer.Position (Position)

evalBuiltinExpr :: EvalBuiltinExprInput -> Maybe (Either String EvalExprResult)
evalBuiltinExpr input = case fname of
  "len" -> Just $ do
    (argVals, argOuts, envAfterArgs) <- evalArgs env fenv args
    case argVals of
      [StringValue {stringValue = s}] -> Right (EvalExprResult {evalExprResultValue = IntValue {intValue = fromIntegral (length s)}, evalExprResultOutputs = argOuts, evalExprResultEnv = envAfterArgs})
      [ListValue {listValueItems = vals}] -> Right (EvalExprResult {evalExprResultValue = IntValue {intValue = fromIntegral (length vals)}, evalExprResultOutputs = argOuts, evalExprResultEnv = envAfterArgs})
      [_] -> Left $ "Type error: len expects string or list at " ++ showPos pos
      _ -> Left $ "Argument count mismatch when calling len at " ++ showPos pos
  "bool" -> Just $ do
    (argVals, argOuts, envAfterArgs) <- evalArgs env fenv args
    case argVals of
      [IntValue {intValue = n}] -> Right (EvalExprResult {evalExprResultValue = IntValue {intValue = if n == 0 then 0 else 1}, evalExprResultOutputs = argOuts, evalExprResultEnv = envAfterArgs})
      [FloatValue {floatValue = n}] -> Right (EvalExprResult {evalExprResultValue = IntValue {intValue = if n == 0 then 0 else 1}, evalExprResultOutputs = argOuts, evalExprResultEnv = envAfterArgs})
      [NoneValue] -> Right (EvalExprResult {evalExprResultValue = IntValue {intValue = 0}, evalExprResultOutputs = argOuts, evalExprResultEnv = envAfterArgs})
      [StringValue {stringValue = s}] -> Right (EvalExprResult {evalExprResultValue = IntValue {intValue = if null s then 0 else 1}, evalExprResultOutputs = argOuts, evalExprResultEnv = envAfterArgs})
      [ListValue {listValueItems = vals}] -> Right (EvalExprResult {evalExprResultValue = IntValue {intValue = if null vals then 0 else 1}, evalExprResultOutputs = argOuts, evalExprResultEnv = envAfterArgs})
      [DictValue {dictValuePairs = pairs}] -> Right (EvalExprResult {evalExprResultValue = IntValue {intValue = if null pairs then 0 else 1}, evalExprResultOutputs = argOuts, evalExprResultEnv = envAfterArgs})
      _ -> Left $ "Argument count mismatch when calling bool at " ++ showPos pos
  "range" -> Just $ do
    (argVals, argOuts, envAfterArgs) <- evalArgs env fenv args
    case argVals of
      [IntValue {intValue = n}] -> Right (EvalExprResult {evalExprResultValue = ListValue {listValueItems = map (\x -> IntValue {intValue = x}) (rangeOne n)}, evalExprResultOutputs = argOuts, evalExprResultEnv = envAfterArgs})
      [IntValue {intValue = start}, IntValue {intValue = stop}] -> Right (EvalExprResult {evalExprResultValue = ListValue {listValueItems = map (\x -> IntValue {intValue = x}) (rangeValues RangeValuesConfig {rangeValuesStart = start, rangeValuesStop = stop, rangeValuesStep = 1})}, evalExprResultOutputs = argOuts, evalExprResultEnv = envAfterArgs})
      [IntValue {intValue = start}, IntValue {intValue = stop}, IntValue {intValue = step}] ->
        if step == 0
          then Left $ "Value error: range step must not be zero at " ++ showPos pos
          else Right (EvalExprResult {evalExprResultValue = ListValue {listValueItems = map (\x -> IntValue {intValue = x}) (rangeValues RangeValuesConfig {rangeValuesStart = start, rangeValuesStop = stop, rangeValuesStep = step})}, evalExprResultOutputs = argOuts, evalExprResultEnv = envAfterArgs})
      [_] -> Left $ "Type error: range expects int at " ++ showPos pos
      [_, _] -> Left $ "Type error: range expects int arguments at " ++ showPos pos
      [_, _, _] -> Left $ "Type error: range expects int arguments at " ++ showPos pos
      _ -> Left $ "Argument count mismatch when calling range at " ++ showPos pos
  "append" -> Just $ do
    (argVals, argOuts, envAfterArgs) <- evalArgs env fenv args
    case argVals of
      [ListValue {listValueItems = vals}, value] -> Right (EvalExprResult {evalExprResultValue = ListValue {listValueItems = vals ++ [value]}, evalExprResultOutputs = argOuts, evalExprResultEnv = envAfterArgs})
      [_, _] -> Left $ "Type error: append expects list as first argument at " ++ showPos pos
      _ -> Left $ "Argument count mismatch when calling append at " ++ showPos pos
  "sort" -> Just $ do
    (argVals, argOuts, envAfterArgs) <- evalArgs env fenv args
    case argVals of
      [ListValue {listValueItems = vals}] ->
        case numberPairs vals of
          Just pairs -> Right (EvalExprResult {evalExprResultValue = ListValue {listValueItems = map snd (sortOn fst pairs)}, evalExprResultOutputs = argOuts, evalExprResultEnv = envAfterArgs})
          Nothing -> Left $ "Type error: sort expects list of number at " ++ showPos pos
      [_] -> Left $ "Type error: sort expects list as first argument at " ++ showPos pos
      _ -> Left $ "Argument count mismatch when calling sort at " ++ showPos pos
  "reverse" -> Just $ do
    (argVals, argOuts, envAfterArgs) <- evalArgs env fenv args
    case argVals of
      [ListValue {listValueItems = vals}] -> Right (EvalExprResult {evalExprResultValue = ListValue {listValueItems = reverse vals}, evalExprResultOutputs = argOuts, evalExprResultEnv = envAfterArgs})
      [_] -> Left $ "Type error: reverse expects list as first argument at " ++ showPos pos
      _ -> Left $ "Argument count mismatch when calling reverse at " ++ showPos pos
  "remove" -> Just $ do
    (argVals, argOuts, envAfterArgs) <- evalArgs env fenv args
    case argVals of
      [ListValue {listValueItems = vals}, target] ->
        case removeFirstValue vals target of
          Just newVals -> Right (EvalExprResult {evalExprResultValue = ListValue {listValueItems = newVals}, evalExprResultOutputs = argOuts, evalExprResultEnv = envAfterArgs})
          Nothing -> Left $ "Value error: remove value not found at " ++ showPos pos
      [_, _] -> Left $ "Type error: remove expects list as first argument at " ++ showPos pos
      _ -> Left $ "Argument count mismatch when calling remove at " ++ showPos pos
  "insert" -> Just $ do
    (argVals, argOuts, envAfterArgs) <- evalArgs env fenv args
    case argVals of
      [ListValue {listValueItems = vals}, IntValue {intValue = index}, value] -> Right (EvalExprResult {evalExprResultValue = ListValue {listValueItems = insertAtIndex InsertAtIndexConfig {insertAtIndexValues = vals, insertAtIndexIndex = index, insertAtIndexValue = value}}, evalExprResultOutputs = argOuts, evalExprResultEnv = envAfterArgs})
      [ListValue {}, _, _] -> Left $ "Type error: insert expects int index at " ++ showPos pos
      [_, _, _] -> Left $ "Type error: insert expects list as first argument at " ++ showPos pos
      _ -> Left $ "Argument count mismatch when calling insert at " ++ showPos pos
  "pop" -> Just $ do
    (argVals, argOuts, envAfterArgs) <- evalArgs env fenv args
    case argVals of
      [ListValue {listValueItems = []}] -> Left $ "Value error: pop from empty list at " ++ showPos pos
      [ListValue {listValueItems = vals}] -> Right (EvalExprResult {evalExprResultValue = last vals, evalExprResultOutputs = argOuts, evalExprResultEnv = envAfterArgs})
      [DictValue {dictValuePairs = pairs}, key] ->
        case lookupDictValue pairs key of
          Just value -> Right (EvalExprResult {evalExprResultValue = value, evalExprResultOutputs = argOuts, evalExprResultEnv = envAfterArgs})
          Nothing -> Left $ "Key not found in pop at " ++ showPos pos
      [DictValue {dictValuePairs = pairs}, key, defaultValue] ->
        case lookupDictValue pairs key of
          Just value -> Right (EvalExprResult {evalExprResultValue = value, evalExprResultOutputs = argOuts, evalExprResultEnv = envAfterArgs})
          Nothing -> Right (EvalExprResult {evalExprResultValue = defaultValue, evalExprResultOutputs = argOuts, evalExprResultEnv = envAfterArgs})
      [_] -> Left $ "Type error: pop expects list at " ++ showPos pos
      [ListValue {}, _] -> Left $ "Argument count mismatch when calling pop at " ++ showPos pos
      [ListValue {}, _, _] -> Left $ "Argument count mismatch when calling pop at " ++ showPos pos
      [_, _] -> Left $ "Type error: pop expects dict as first argument at " ++ showPos pos
      [_, _, _] -> Left $ "Type error: pop expects dict as first argument at " ++ showPos pos
      _ -> Left $ "Argument count mismatch when calling pop at " ++ showPos pos
  "clear" -> Just $ do
    (argVals, argOuts, envAfterArgs) <- evalArgs env fenv args
    case argVals of
      [ListValue {}] -> Right (EvalExprResult {evalExprResultValue = ListValue {listValueItems = []}, evalExprResultOutputs = argOuts, evalExprResultEnv = envAfterArgs})
      [DictValue {}] -> Right (EvalExprResult {evalExprResultValue = DictValue {dictValuePairs = []}, evalExprResultOutputs = argOuts, evalExprResultEnv = envAfterArgs})
      [_] -> Left $ "Type error: clear expects list or dict at " ++ showPos pos
      _ -> Left $ "Argument count mismatch when calling clear at " ++ showPos pos
  "keys" -> Just $ do
    (argVals, argOuts, envAfterArgs) <- evalArgs env fenv args
    case argVals of
      [DictValue {dictValuePairs = pairs}] -> Right (EvalExprResult {evalExprResultValue = ListValue {listValueItems = map fst pairs}, evalExprResultOutputs = argOuts, evalExprResultEnv = envAfterArgs})
      [_] -> Left $ "Type error: keys expects dict at " ++ showPos pos
      _ -> Left $ "Argument count mismatch when calling keys at " ++ showPos pos
  "get" -> Just $ do
    (argVals, argOuts, envAfterArgs) <- evalArgs env fenv args
    case argVals of
      [DictValue {dictValuePairs = pairs}, key] ->
        case lookupDictValue pairs key of
          Just value -> Right (EvalExprResult {evalExprResultValue = value, evalExprResultOutputs = argOuts, evalExprResultEnv = envAfterArgs})
          Nothing -> Left $ "Key not found in get at " ++ showPos pos
      [DictValue {dictValuePairs = pairs}, key, defaultValue] ->
        case lookupDictValue pairs key of
          Just value -> Right (EvalExprResult {evalExprResultValue = value, evalExprResultOutputs = argOuts, evalExprResultEnv = envAfterArgs})
          Nothing -> Right (EvalExprResult {evalExprResultValue = defaultValue, evalExprResultOutputs = argOuts, evalExprResultEnv = envAfterArgs})
      [_, _] -> Left $ "Type error: get expects dict as first argument at " ++ showPos pos
      [_, _, _] -> Left $ "Type error: get expects dict as first argument at " ++ showPos pos
      _ -> Left $ "Argument count mismatch when calling get at " ++ showPos pos
  "update" -> Just $ do
    (argVals, argOuts, envAfterArgs) <- evalArgs env fenv args
    case argVals of
      [DictValue {dictValuePairs = pairs}, DictValue {dictValuePairs = otherPairs}] -> Right (EvalExprResult {evalExprResultValue = DictValue {dictValuePairs = mergeDictValues pairs otherPairs}, evalExprResultOutputs = argOuts, evalExprResultEnv = envAfterArgs})
      [DictValue {}, _] -> Left $ "Type error: update expects dict as second argument at " ++ showPos pos
      [DictValue {dictValuePairs = pairs}, key, value] -> Right (EvalExprResult {evalExprResultValue = DictValue {dictValuePairs = updateDictValue pairs key value}, evalExprResultOutputs = argOuts, evalExprResultEnv = envAfterArgs})
      [_, _, _] -> Left $ "Type error: update expects dict as first argument at " ++ showPos pos
      [_, _] -> Left $ "Type error: update expects dict as first argument at " ++ showPos pos
      _ -> Left $ "Argument count mismatch when calling update at " ++ showPos pos
  "setdefault" -> Just $ do
    (argVals, argOuts, envAfterArgs) <- evalArgs env fenv args
    case argVals of
      [DictValue {dictValuePairs = pairs}, key] -> Right (EvalExprResult {evalExprResultValue = DictValue {dictValuePairs = setDefaultDictValue pairs key NoneValue}, evalExprResultOutputs = argOuts, evalExprResultEnv = envAfterArgs})
      [DictValue {dictValuePairs = pairs}, key, defaultValue] -> Right (EvalExprResult {evalExprResultValue = DictValue {dictValuePairs = setDefaultDictValue pairs key defaultValue}, evalExprResultOutputs = argOuts, evalExprResultEnv = envAfterArgs})
      [_, _] -> Left $ "Type error: setdefault expects dict as first argument at " ++ showPos pos
      [_, _, _] -> Left $ "Type error: setdefault expects dict as first argument at " ++ showPos pos
      _ -> Left $ "Argument count mismatch when calling setdefault at " ++ showPos pos
  "values" -> Just $ do
    (argVals, argOuts, envAfterArgs) <- evalArgs env fenv args
    case argVals of
      [DictValue {dictValuePairs = pairs}] -> Right (EvalExprResult {evalExprResultValue = ListValue {listValueItems = map snd pairs}, evalExprResultOutputs = argOuts, evalExprResultEnv = envAfterArgs})
      [_] -> Left $ "Type error: values expects dict at " ++ showPos pos
      _ -> Left $ "Argument count mismatch when calling values at " ++ showPos pos
  "items" -> Just $ do
    (argVals, argOuts, envAfterArgs) <- evalArgs env fenv args
    case argVals of
      [DictValue {dictValuePairs = pairs}] -> Right (EvalExprResult {evalExprResultValue = ListValue {listValueItems = map pairToList pairs}, evalExprResultOutputs = argOuts, evalExprResultEnv = envAfterArgs})
      [_] -> Left $ "Type error: items expects dict at " ++ showPos pos
      _ -> Left $ "Argument count mismatch when calling items at " ++ showPos pos
  _ -> Nothing
  where
    evalExprFn = evalBuiltinExprEvalExprFn input
    env = evalBuiltinExprEnv input
    fenv = evalBuiltinExprFuncEnv input
    fname = evalBuiltinExprFname input
    args = evalBuiltinExprArgs input
    pos = evalBuiltinExprPos input

    evalArgs currentEnv currentFenv = foldl go (Right ([], [], currentEnv))
      where
        go acc expr = do
          (vals, outs, envNow) <- acc
          EvalExprResult {evalExprResultValue = value, evalExprResultOutputs = exprOuts, evalExprResultEnv = envNext} <- evalExprFn envNow currentFenv expr
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
    numberPairs (IntValue {intValue = n} : restVals) = fmap ((fromIntegral n, IntValue {intValue = n}) :) (numberPairs restVals)
    numberPairs (FloatValue {floatValue = n} : restVals) = fmap ((n, FloatValue {floatValue = n}) :) (numberPairs restVals)
    numberPairs (_ : _) = Nothing
