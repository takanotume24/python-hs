module PythonHS.Evaluator.EvalAssignStmt (evalAssignStmt) where

import Data.Map.Strict qualified as Map
import PythonHS.Evaluator.ComputeAssignInput (ComputeAssignInput (..))
import PythonHS.Evaluator.EvalAssignStmtInput (EvalAssignStmtInput (..))
import PythonHS.Evaluator.EvalExprResult (EvalExprResult (..))
import PythonHS.Evaluator.EvalStmtResult (EvalStmtResult)
import PythonHS.Evaluator.ShowPos (showPos)

evalAssignStmt :: EvalAssignStmtInput -> Either String EvalStmtResult
evalAssignStmt input =
  let env = evalAssignStmtInputEnv input
      name = evalAssignStmtInputName input
      pos = evalAssignStmtInputPos input
   in case Map.lookup name env of
        Nothing -> Left $ "Name error: undefined identifier " ++ name ++ " at " ++ showPos pos
        Just current -> do
          EvalExprResult {evalExprResultValue = rhs, evalExprResultOutputs = exprOuts, evalExprResultEnv = envAfterExpr} <- evalAssignStmtInputEvalExpr input (evalAssignStmtInputValueExpr input)
          newValue <- evalAssignStmtInputCompute input ComputeAssignInput {computeAssignInputOp = evalAssignStmtInputOp input, computeAssignInputPos = pos, computeAssignInputLeft = current, computeAssignInputRight = rhs}
          evalAssignStmtInputEvalStatements input (Map.insert name newValue envAfterExpr) (evalAssignStmtInputFuncEnv input) (evalAssignStmtInputOutputs input ++ exprOuts) (evalAssignStmtInputRest input)
