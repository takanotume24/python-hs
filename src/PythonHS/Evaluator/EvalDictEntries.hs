module PythonHS.Evaluator.EvalDictEntries (evalDictEntries) where

import PythonHS.AST.DictEntry (DictEntry (..))
import PythonHS.Evaluator.DictPair (DictPair (..))
import PythonHS.Evaluator.EvalDictEntriesInput (EvalDictEntriesInput (..))
import PythonHS.Evaluator.EvalExprResult (EvalExprResult (..))
import PythonHS.Evaluator.EvaluatedArgsResult (EvaluatedArgsResult (..))
import PythonHS.Evaluator.Value (Value)

evalDictEntries :: EvalDictEntriesInput -> Either String (EvaluatedArgsResult (DictPair Value Value))
evalDictEntries input =
  case evalDictEntriesInputEntries input of
    [] -> Right EvaluatedArgsResult {evaluatedArgsResultValues = [], evaluatedArgsResultOutputs = [], evaluatedArgsResultEnv = evalDictEntriesInputEnv input}
    (DictEntry {dictEntryKey = keyExpr, dictEntryValue = valueExpr} : restEntries) -> do
      let currentEnv = evalDictEntriesInputEnv input
          currentFenv = evalDictEntriesInputFuncEnv input
          evalExprFn = evalDictEntriesInputEvalExpr input
      EvalExprResult keyVal keyOuts envAfterKey <- evalExprFn currentEnv currentFenv keyExpr
      EvalExprResult valueVal valueOuts envAfterValue <- evalExprFn envAfterKey currentFenv valueExpr
      restResult <- evalDictEntries input {evalDictEntriesInputEnv = envAfterValue, evalDictEntriesInputEntries = restEntries}
      Right EvaluatedArgsResult {evaluatedArgsResultValues = DictPair {dictPairKey = keyVal, dictPairValue = valueVal} : evaluatedArgsResultValues restResult, evaluatedArgsResultOutputs = keyOuts ++ valueOuts ++ evaluatedArgsResultOutputs restResult, evaluatedArgsResultEnv = evaluatedArgsResultEnv restResult}
