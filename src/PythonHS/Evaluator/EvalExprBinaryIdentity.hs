module PythonHS.Evaluator.EvalExprBinaryIdentity (evalExprBinaryIdentity) where

import PythonHS.Evaluator.EvalExprBinaryIdentityConfig (EvalExprBinaryIdentityConfig (..))
import PythonHS.Evaluator.EvalExprResult (EvalExprResult (..))
import PythonHS.Evaluator.Value (Value (..))

evalExprBinaryIdentity :: EvalExprBinaryIdentityConfig -> Either String EvalExprResult
evalExprBinaryIdentity config = do
  let evalExprFn = evalExprBinaryIdentityConfigEvalExprFn config
      env = evalExprBinaryIdentityConfigEnv config
      fenv = evalExprBinaryIdentityConfigFuncEnv config
      leftExpr = evalExprBinaryIdentityConfigLeftExpr config
      rightExpr = evalExprBinaryIdentityConfigRightExpr config
      negateResult = evalExprBinaryIdentityConfigNegate config
  EvalExprResult {evalExprResultValue = leftVal, evalExprResultOutputs = leftOuts, evalExprResultEnv = envAfterLeft} <- evalExprFn env fenv leftExpr
  EvalExprResult {evalExprResultValue = rightVal, evalExprResultOutputs = rightOuts, evalExprResultEnv = envAfterRight} <- evalExprFn envAfterLeft fenv rightExpr
  let same = leftVal == rightVal
      result = if negateResult then not same else same
  Right (EvalExprResult {evalExprResultValue = IntValue {intValue = if result then 1 else 0}, evalExprResultOutputs = leftOuts ++ rightOuts, evalExprResultEnv = envAfterRight})
