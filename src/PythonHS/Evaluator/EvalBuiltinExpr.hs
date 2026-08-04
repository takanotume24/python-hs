module PythonHS.Evaluator.EvalBuiltinExpr (evalBuiltinExpr) where

import Data.List (sortOn)
import PythonHS.Evaluator.DictPair (DictPair (..))
import PythonHS.Evaluator.EvalBuiltinExprInput (EvalBuiltinExprInput (..))
import PythonHS.Evaluator.EvalExprResult (EvalExprResult (..))
import PythonHS.Evaluator.EvaluatedArgsResult (EvaluatedArgsResult (..))
import PythonHS.Evaluator.InsertAtIndex (insertAtIndex)
import PythonHS.Evaluator.InsertAtIndexConfig (InsertAtIndexConfig (..))
import PythonHS.Evaluator.LookupDictValue (lookupDictValue)
import PythonHS.Evaluator.LookupDictValueConfig (LookupDictValueConfig (..))
import PythonHS.Evaluator.NumberPairs (numberPairs)
import PythonHS.Evaluator.NumberPairsConfig (NumberPairsConfig (..))
import PythonHS.Evaluator.NumberSortPair (NumberSortPair (..))
import PythonHS.Evaluator.PairToList (pairToList)
import PythonHS.Evaluator.RangeOne (rangeOne)
import PythonHS.Evaluator.RangeValues (rangeValues)
import PythonHS.Evaluator.RangeValuesConfig (RangeValuesConfig (..))
import PythonHS.Evaluator.RemoveFirstValue (removeFirstValue)
import PythonHS.Evaluator.RemoveFirstValueConfig (RemoveFirstValueConfig (..))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.UpdateDictValue (updateDictValue)
import PythonHS.Evaluator.UpdateDictValueConfig (UpdateDictValueConfig (..))
import PythonHS.Evaluator.Value (Value (..))

evalBuiltinExpr :: EvalBuiltinExprInput -> Maybe (Either String EvalExprResult)
evalBuiltinExpr input = case fname of
  "len" -> Just $ do
    argsResult <- evalArgs env fenv args
    case evaluatedArgsResultValues argsResult of
      [StringValue {stringValue = s}] -> Right (mkResult argsResult (IntValue {intValue = fromIntegral (length s)}))
      [ListValue {listValueItems = vals}] -> Right (mkResult argsResult (IntValue {intValue = fromIntegral (length vals)}))
      [_] -> Left $ "Type error: len expects string or list at " ++ showPos pos
      _ -> Left $ "Argument count mismatch when calling len at " ++ showPos pos
  "bool" -> Just $ do
    argsResult <- evalArgs env fenv args
    case evaluatedArgsResultValues argsResult of
      [IntValue {intValue = n}] -> Right (mkResult argsResult (IntValue {intValue = if n == 0 then 0 else 1}))
      [FloatValue {floatValue = n}] -> Right (mkResult argsResult (IntValue {intValue = if n == 0 then 0 else 1}))
      [NoneValue] -> Right (mkResult argsResult (IntValue {intValue = 0}))
      [StringValue {stringValue = s}] -> Right (mkResult argsResult (IntValue {intValue = if null s then 0 else 1}))
      [ListValue {listValueItems = vals}] -> Right (mkResult argsResult (IntValue {intValue = if null vals then 0 else 1}))
      [DictValue {dictValuePairs = pairs}] -> Right (mkResult argsResult (IntValue {intValue = if null pairs then 0 else 1}))
      _ -> Left $ "Argument count mismatch when calling bool at " ++ showPos pos
  "range" -> Just $ do
    argsResult <- evalArgs env fenv args
    case evaluatedArgsResultValues argsResult of
      [IntValue {intValue = n}] -> Right (mkResult argsResult (ListValue {listValueItems = map (\x -> IntValue {intValue = x}) (rangeOne n)}))
      [IntValue {intValue = start}, IntValue {intValue = stop}] -> Right (mkResult argsResult (ListValue {listValueItems = map (\x -> IntValue {intValue = x}) (rangeValues RangeValuesConfig {rangeValuesStart = start, rangeValuesStop = stop, rangeValuesStep = 1})}))
      [IntValue {intValue = start}, IntValue {intValue = stop}, IntValue {intValue = step}] ->
        if step == 0
          then Left $ "Value error: range step must not be zero at " ++ showPos pos
          else Right (mkResult argsResult (ListValue {listValueItems = map (\x -> IntValue {intValue = x}) (rangeValues RangeValuesConfig {rangeValuesStart = start, rangeValuesStop = stop, rangeValuesStep = step})}))
      [_] -> Left $ "Type error: range expects int at " ++ showPos pos
      [_, _] -> Left $ "Type error: range expects int arguments at " ++ showPos pos
      [_, _, _] -> Left $ "Type error: range expects int arguments at " ++ showPos pos
      _ -> Left $ "Argument count mismatch when calling range at " ++ showPos pos
  "append" -> Just $ do
    argsResult <- evalArgs env fenv args
    case evaluatedArgsResultValues argsResult of
      [ListValue {listValueItems = vals}, value] -> Right (mkResult argsResult (ListValue {listValueItems = vals ++ [value]}))
      [_, _] -> Left $ "Type error: append expects list as first argument at " ++ showPos pos
      _ -> Left $ "Argument count mismatch when calling append at " ++ showPos pos
  "sort" -> Just $ do
    argsResult <- evalArgs env fenv args
    case evaluatedArgsResultValues argsResult of
      [ListValue {listValueItems = vals}] ->
        case numberPairs NumberPairsConfig {numberPairsConfigValues = vals} of
          Just pairs ->
            let sortedVals = map numberSortPairValue (sortOn numberSortPairSortKey pairs)
             in Right (mkResult argsResult (ListValue {listValueItems = sortedVals}))
          Nothing -> Left $ "Type error: sort expects list of number at " ++ showPos pos
      [_] -> Left $ "Type error: sort expects list as first argument at " ++ showPos pos
      _ -> Left $ "Argument count mismatch when calling sort at " ++ showPos pos
  "reverse" -> Just $ do
    argsResult <- evalArgs env fenv args
    case evaluatedArgsResultValues argsResult of
      [ListValue {listValueItems = vals}] -> Right (mkResult argsResult (ListValue {listValueItems = reverse vals}))
      [_] -> Left $ "Type error: reverse expects list as first argument at " ++ showPos pos
      _ -> Left $ "Argument count mismatch when calling reverse at " ++ showPos pos
  "remove" -> Just $ do
    argsResult <- evalArgs env fenv args
    case evaluatedArgsResultValues argsResult of
      [ListValue {listValueItems = vals}, target] ->
        case removeFirstValue RemoveFirstValueConfig {removeFirstValueConfigValues = vals, removeFirstValueConfigTarget = target} of
          Just newVals -> Right (mkResult argsResult (ListValue {listValueItems = newVals}))
          Nothing -> Left $ "Value error: remove value not found at " ++ showPos pos
      [_, _] -> Left $ "Type error: remove expects list as first argument at " ++ showPos pos
      _ -> Left $ "Argument count mismatch when calling remove at " ++ showPos pos
  "insert" -> Just $ do
    argsResult <- evalArgs env fenv args
    case evaluatedArgsResultValues argsResult of
      [ListValue {listValueItems = vals}, IntValue {intValue = index}, value] -> Right (mkResult argsResult (ListValue {listValueItems = insertAtIndex InsertAtIndexConfig {insertAtIndexValues = vals, insertAtIndexIndex = index, insertAtIndexValue = value}}))
      [ListValue {}, _, _] -> Left $ "Type error: insert expects int index at " ++ showPos pos
      [_, _, _] -> Left $ "Type error: insert expects list as first argument at " ++ showPos pos
      _ -> Left $ "Argument count mismatch when calling insert at " ++ showPos pos
  "pop" -> Just $ do
    argsResult <- evalArgs env fenv args
    case evaluatedArgsResultValues argsResult of
      [ListValue {listValueItems = []}] -> Left $ "Value error: pop from empty list at " ++ showPos pos
      [ListValue {listValueItems = vals}] -> Right (mkResult argsResult (last vals))
      [DictValue {dictValuePairs = pairs}, key] ->
        case lookupDictValue LookupDictValueConfig {lookupDictValueConfigPairs = pairs, lookupDictValueConfigKey = key} of
          Just value -> Right (mkResult argsResult value)
          Nothing -> Left $ "Key not found in pop at " ++ showPos pos
      [DictValue {dictValuePairs = pairs}, key, defaultValue] ->
        case lookupDictValue LookupDictValueConfig {lookupDictValueConfigPairs = pairs, lookupDictValueConfigKey = key} of
          Just value -> Right (mkResult argsResult value)
          Nothing -> Right (mkResult argsResult defaultValue)
      [_] -> Left $ "Type error: pop expects list at " ++ showPos pos
      [ListValue {}, _] -> Left $ "Argument count mismatch when calling pop at " ++ showPos pos
      [ListValue {}, _, _] -> Left $ "Argument count mismatch when calling pop at " ++ showPos pos
      [_, _] -> Left $ "Type error: pop expects dict as first argument at " ++ showPos pos
      [_, _, _] -> Left $ "Type error: pop expects dict as first argument at " ++ showPos pos
      _ -> Left $ "Argument count mismatch when calling pop at " ++ showPos pos
  "clear" -> Just $ do
    argsResult <- evalArgs env fenv args
    case evaluatedArgsResultValues argsResult of
      [ListValue {}] -> Right (mkResult argsResult (ListValue {listValueItems = []}))
      [DictValue {}] -> Right (mkResult argsResult (DictValue {dictValuePairs = []}))
      [_] -> Left $ "Type error: clear expects list or dict at " ++ showPos pos
      _ -> Left $ "Argument count mismatch when calling clear at " ++ showPos pos
  "keys" -> Just $ do
    argsResult <- evalArgs env fenv args
    case evaluatedArgsResultValues argsResult of
      [DictValue {dictValuePairs = pairs}] -> Right (mkResult argsResult (ListValue {listValueItems = map dictPairKey pairs}))
      [_] -> Left $ "Type error: keys expects dict at " ++ showPos pos
      _ -> Left $ "Argument count mismatch when calling keys at " ++ showPos pos
  "get" -> Just $ do
    argsResult <- evalArgs env fenv args
    case evaluatedArgsResultValues argsResult of
      [DictValue {dictValuePairs = pairs}, key] ->
        case lookupDictValue LookupDictValueConfig {lookupDictValueConfigPairs = pairs, lookupDictValueConfigKey = key} of
          Just value -> Right (mkResult argsResult value)
          Nothing -> Left $ "Key not found in get at " ++ showPos pos
      [DictValue {dictValuePairs = pairs}, key, defaultValue] ->
        case lookupDictValue LookupDictValueConfig {lookupDictValueConfigPairs = pairs, lookupDictValueConfigKey = key} of
          Just value -> Right (mkResult argsResult value)
          Nothing -> Right (mkResult argsResult defaultValue)
      [_, _] -> Left $ "Type error: get expects dict as first argument at " ++ showPos pos
      [_, _, _] -> Left $ "Type error: get expects dict as first argument at " ++ showPos pos
      _ -> Left $ "Argument count mismatch when calling get at " ++ showPos pos
  "update" -> Just $ do
    argsResult <- evalArgs env fenv args
    case evaluatedArgsResultValues argsResult of
      [DictValue {dictValuePairs = pairs}, DictValue {dictValuePairs = otherPairs}] -> Right (mkResult argsResult (DictValue {dictValuePairs = mergeDictValues pairs otherPairs}))
      [DictValue {}, _] -> Left $ "Type error: update expects dict as second argument at " ++ showPos pos
      [DictValue {dictValuePairs = pairs}, key, value] -> Right (mkResult argsResult (DictValue {dictValuePairs = updateDictValue UpdateDictValueConfig {updateDictValueConfigPairs = pairs, updateDictValueConfigKey = key, updateDictValueConfigValue = value}}))
      [_, _, _] -> Left $ "Type error: update expects dict as first argument at " ++ showPos pos
      [_, _] -> Left $ "Type error: update expects dict as first argument at " ++ showPos pos
      _ -> Left $ "Argument count mismatch when calling update at " ++ showPos pos
  "setdefault" -> Just $ do
    argsResult <- evalArgs env fenv args
    case evaluatedArgsResultValues argsResult of
      [DictValue {dictValuePairs = pairs}, key] -> Right (mkResult argsResult (DictValue {dictValuePairs = setDefaultDictValue pairs key NoneValue}))
      [DictValue {dictValuePairs = pairs}, key, defaultValue] -> Right (mkResult argsResult (DictValue {dictValuePairs = setDefaultDictValue pairs key defaultValue}))
      [_, _] -> Left $ "Type error: setdefault expects dict as first argument at " ++ showPos pos
      [_, _, _] -> Left $ "Type error: setdefault expects dict as first argument at " ++ showPos pos
      _ -> Left $ "Argument count mismatch when calling setdefault at " ++ showPos pos
  "values" -> Just $ do
    argsResult <- evalArgs env fenv args
    case evaluatedArgsResultValues argsResult of
      [DictValue {dictValuePairs = pairs}] -> Right (mkResult argsResult (ListValue {listValueItems = map dictPairValue pairs}))
      [_] -> Left $ "Type error: values expects dict at " ++ showPos pos
      _ -> Left $ "Argument count mismatch when calling values at " ++ showPos pos
  "items" -> Just $ do
    argsResult <- evalArgs env fenv args
    case evaluatedArgsResultValues argsResult of
      [DictValue {dictValuePairs = pairs}] -> Right (mkResult argsResult (ListValue {listValueItems = map pairToList pairs}))
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

    mkResult argsResult val =
      EvalExprResult {evalExprResultValue = val, evalExprResultOutputs = evaluatedArgsResultOutputs argsResult, evalExprResultEnv = evaluatedArgsResultEnv argsResult}

    evalArgs currentEnv currentFenv = foldl go (Right EvaluatedArgsResult {evaluatedArgsResultValues = [], evaluatedArgsResultOutputs = [], evaluatedArgsResultEnv = currentEnv})
      where
        go acc expr = do
          result <- acc
          EvalExprResult {evalExprResultValue = value, evalExprResultOutputs = exprOuts, evalExprResultEnv = envNext} <- evalExprFn (evaluatedArgsResultEnv result) currentFenv expr
          Right EvaluatedArgsResult {evaluatedArgsResultValues = evaluatedArgsResultValues result ++ [value], evaluatedArgsResultOutputs = evaluatedArgsResultOutputs result ++ exprOuts, evaluatedArgsResultEnv = envNext}

    mergeDictValues pairs [] = pairs
    mergeDictValues pairs (entry : restPairs) =
      mergeDictValues (updateDictValue UpdateDictValueConfig {updateDictValueConfigPairs = pairs, updateDictValueConfigKey = dictPairKey entry, updateDictValueConfigValue = dictPairValue entry}) restPairs

    setDefaultDictValue pairs key defaultValue =
      case lookupDictValue LookupDictValueConfig {lookupDictValueConfigPairs = pairs, lookupDictValueConfigKey = key} of
        Just _ -> pairs
        Nothing -> pairs ++ [DictPair {dictPairKey = key, dictPairValue = defaultValue}]
