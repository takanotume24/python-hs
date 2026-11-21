module PythonHS.Evaluator.EvalComparisonItems (evalComparisonItems) where

import PythonHS.Evaluator.EvalComparisonItemsInput (EvalComparisonItemsInput (..))
import PythonHS.Evaluator.EvalExprResult (EvalExprResult (..))
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.EvalBinaryOp (evalBinaryOp)
import PythonHS.VM.EvalBinaryOpConfig (EvalBinaryOpConfig (..))

evalComparisonItems :: EvalComparisonItemsInput -> Either String EvalExprResult
evalComparisonItems itemsInput =
  case evalComparisonItemsInputRemainingOps itemsInput of
    [] -> Right (EvalExprResult {evalExprResultValue = IntValue 1, evalExprResultOutputs = evalComparisonItemsInputAccOutputs itemsInput, evalExprResultEnv = evalComparisonItemsInputEnv itemsInput})
    (op : restOps) ->
      case evalComparisonItemsInputRemainingRights itemsInput of
        [] -> Right (EvalExprResult {evalExprResultValue = IntValue 1, evalExprResultOutputs = evalComparisonItemsInputAccOutputs itemsInput, evalExprResultEnv = evalComparisonItemsInputEnv itemsInput})
        (rightExpr : restRights) -> do
          let evalExprFn = evalComparisonItemsInputEvalExpr itemsInput
              currentFenv = evalComparisonItemsInputFuncEnv itemsInput
              leftVal = evalComparisonItemsInputLeftValue itemsInput
              accOuts = evalComparisonItemsInputAccOutputs itemsInput
              envAfter = evalComparisonItemsInputEnv itemsInput
              pos' = evalComparisonItemsInputPos itemsInput
          EvalExprResult {evalExprResultValue = rightVal, evalExprResultOutputs = rightOuts, evalExprResultEnv = envAfterRight} <- evalExprFn envAfter currentFenv rightExpr
          case evalBinaryOp EvalBinaryOpConfig {evalBinaryOpOp = op, evalBinaryOpLeft = leftVal, evalBinaryOpRight = rightVal, evalBinaryOpPos = pos'} of
            Left err -> Left err
            Right resultVal ->
              case resultVal of
                IntValue {intValue = 0} -> Right (EvalExprResult {evalExprResultValue = IntValue 0, evalExprResultOutputs = accOuts ++ rightOuts, evalExprResultEnv = envAfterRight})
                _ ->
                  evalComparisonItems
                    itemsInput
                      { evalComparisonItemsInputLeftValue = rightVal,
                        evalComparisonItemsInputAccOutputs = accOuts ++ rightOuts,
                        evalComparisonItemsInputEnv = envAfterRight,
                        evalComparisonItemsInputRemainingOps = restOps,
                        evalComparisonItemsInputRemainingRights = restRights
                      }
