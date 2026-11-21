module PythonHS.Evaluator.EvalComparisonChain (evalComparisonChain) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalComparisonChainInput (EvalComparisonChainInput (..))
import PythonHS.Evaluator.EvalComparisonItems (evalComparisonItems)
import PythonHS.Evaluator.EvalComparisonItemsInput (EvalComparisonItemsInput (..))
import PythonHS.Evaluator.EvalExprInput (EvalExprInput (..))
import PythonHS.Evaluator.EvalExprResult (EvalExprResult (..))
import PythonHS.Evaluator.EvalStmtResult (EvalStmtResult)
import PythonHS.Evaluator.FuncEnv (FuncEnv)

evalComparisonChain :: EvalComparisonChainInput -> Either String EvalExprResult
evalComparisonChain chainInput = do
  let evalExprFn = evalComparisonChainInputEvalExpr chainInput
      evalStatementsFn' = evalComparisonChainInputEvalStatementsFn chainInput
      currentEnv = evalComparisonChainInputEnv chainInput
      currentFenv = evalComparisonChainInputFuncEnv chainInput
      leftExpr = evalComparisonChainInputLeft chainInput
      pos' = evalComparisonChainInputPos chainInput
  EvalExprResult {evalExprResultValue = leftVal, evalExprResultOutputs = leftOuts, evalExprResultEnv = envAfterLeft} <- evalExprFn currentEnv currentFenv leftExpr
  evalComparisonItems
    EvalComparisonItemsInput
      { evalComparisonItemsInputEvalStatementsFn = evalStatementsFn',
        evalComparisonItemsInputEvalExpr = evalExprFn,
        evalComparisonItemsInputFuncEnv = currentFenv,
        evalComparisonItemsInputLeftValue = leftVal,
        evalComparisonItemsInputAccOutputs = leftOuts,
        evalComparisonItemsInputEnv = envAfterLeft,
        evalComparisonItemsInputRemainingOps = evalComparisonChainInputOps chainInput,
        evalComparisonItemsInputRemainingRights = evalComparisonChainInputRights chainInput,
        evalComparisonItemsInputPos = pos'
      }
