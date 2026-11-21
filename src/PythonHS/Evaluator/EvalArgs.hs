module PythonHS.Evaluator.EvalArgs (evalArgs) where

import PythonHS.Evaluator.EvalArgsGoInput (EvalArgsGoInput (..))
import PythonHS.Evaluator.EvalArgsInput (EvalArgsInput (..))
import PythonHS.Evaluator.EvalExprResult (EvalExprResult (..))
import PythonHS.Evaluator.EvaluatedArgsResult (EvaluatedArgsResult (..))
import PythonHS.Evaluator.Value (Value)

evalArgs :: EvalArgsInput -> Either String (EvaluatedArgsResult Value)
evalArgs input = go EvalArgsGoInput {evalArgsGoInputEvalExpr = evalArgsInputEvalExpr input, evalArgsGoInputFuncEnv = evalArgsInputFuncEnv input, evalArgsGoInputAcc = Right EvaluatedArgsResult {evaluatedArgsResultValues = [], evaluatedArgsResultOutputs = [], evaluatedArgsResultEnv = evalArgsInputEnv input}, evalArgsGoInputExprs = evalArgsInputExprs input}
  where
    go :: EvalArgsGoInput -> Either String (EvaluatedArgsResult Value)
    go goInput = do
      result <- evalArgsGoInputAcc goInput
      case evalArgsGoInputExprs goInput of
        [] -> Right result
        (argExpr : restExprs) -> do
          EvalExprResult value exprOuts envNext <- evalArgsGoInputEvalExpr goInput (evaluatedArgsResultEnv result) (evalArgsGoInputFuncEnv goInput) argExpr
          go EvalArgsGoInput {evalArgsGoInputEvalExpr = evalArgsGoInputEvalExpr goInput, evalArgsGoInputFuncEnv = evalArgsGoInputFuncEnv goInput, evalArgsGoInputAcc = Right EvaluatedArgsResult {evaluatedArgsResultValues = evaluatedArgsResultValues result ++ [value], evaluatedArgsResultOutputs = evaluatedArgsResultOutputs result ++ exprOuts, evaluatedArgsResultEnv = envNext}, evalArgsGoInputExprs = restExprs}
