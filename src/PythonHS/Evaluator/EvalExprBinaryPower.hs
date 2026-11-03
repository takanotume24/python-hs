module PythonHS.Evaluator.EvalExprBinaryPower (evalExprBinaryPower) where

import PythonHS.Evaluator.EvalExprBinaryPowerConfig (EvalExprBinaryPowerConfig (..))
import PythonHS.Evaluator.EvalExprResult (EvalExprResult (..))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))

evalExprBinaryPower :: EvalExprBinaryPowerConfig -> Either String EvalExprResult
evalExprBinaryPower config = do
  EvalExprResult {evalExprResultValue = leftVal, evalExprResultOutputs = leftOuts, evalExprResultEnv = envAfterLeft} <- evalExprFn env fenv leftExpr
  EvalExprResult {evalExprResultValue = rightVal, evalExprResultOutputs = rightOuts, evalExprResultEnv = envAfterRight} <- evalExprFn envAfterLeft fenv rightExpr
  case leftVal of
    IntValue {intValue = leftInt} ->
      case rightVal of
        IntValue {intValue = rightInt} ->
          if rightInt >= 0
            then Right (EvalExprResult {evalExprResultValue = IntValue {intValue = leftInt ^ rightInt}, evalExprResultOutputs = leftOuts ++ rightOuts, evalExprResultEnv = envAfterRight})
            else
              if leftInt == 0
                then Left $ "Value error: 0.0 cannot be raised to a negative power at " ++ showPos pos
                else Right (EvalExprResult {evalExprResultValue = FloatValue {floatValue = fromIntegral leftInt ** fromIntegral rightInt}, evalExprResultOutputs = leftOuts ++ rightOuts, evalExprResultEnv = envAfterRight})
        _ -> evalPowerFloat leftVal rightVal leftOuts rightOuts envAfterRight pos
    _ -> evalPowerFloat leftVal rightVal leftOuts rightOuts envAfterRight pos
  where
    evalExprFn = evalExprBinaryPowerConfigEvalExprFn config
    env = evalExprBinaryPowerConfigEnv config
    fenv = evalExprBinaryPowerConfigFuncEnv config
    leftExpr = evalExprBinaryPowerConfigLeftExpr config
    rightExpr = evalExprBinaryPowerConfigRightExpr config
    pos = evalExprBinaryPowerConfigPos config
    expectNumber = evalExprBinaryPowerConfigExpectNumber config

    evalPowerFloat l r outsL outsR env' pos'' = do
      leftNumber <- expectNumber pos'' l
      rightNumber <- expectNumber pos'' r
      if leftNumber == 0 && rightNumber < 0
        then Left $ "Value error: 0.0 cannot be raised to a negative power at " ++ showPos pos''
        else Right (EvalExprResult {evalExprResultValue = FloatValue {floatValue = leftNumber ** rightNumber}, evalExprResultOutputs = outsL ++ outsR, evalExprResultEnv = env'})
