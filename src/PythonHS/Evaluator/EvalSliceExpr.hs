module PythonHS.Evaluator.EvalSliceExpr (evalSliceExpr) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalExprResult (EvalExprResult (..))
import PythonHS.Evaluator.EvalSliceExprInput (EvalSliceExprInput (..))
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Evaluator.Value (Value (NoneValue))
import PythonHS.VM.SliceValue (sliceValue)
import PythonHS.VM.SliceValueConfig (SliceValueConfig (..))

evalSliceExpr :: EvalSliceExprInput -> Either String EvalExprResult
evalSliceExpr input = do
  EvalExprResult baseVal baseOuts envAfterBase <- evalExprFn env0 fenv baseExpr
  (startVal, startOuts, envAfterStart) <- evalBound (evalSliceExprInputStart input) envAfterBase
  (endVal, endOuts, envAfterEnd) <- evalBound (evalSliceExprInputEnd input) envAfterStart
  resultVal <- sliceValue SliceValueConfig {sliceValuePos = pos, sliceValueSeqValue = baseVal, sliceValueStartVal = startVal, sliceValueEndVal = endVal}
  Right (EvalExprResult resultVal (baseOuts ++ startOuts ++ endOuts) envAfterEnd)
  where
    evalExprFn = evalSliceExprInputEvalExpr input
    fenv = evalSliceExprInputFuncEnv input
    pos = evalSliceExprInputPos input
    env0 = evalSliceExprInputEnv input
    baseExpr = evalSliceExprInputBase input

    evalBound :: Maybe Expr -> Env -> Either String (Value, [String], Env)
    evalBound Nothing env' = Right (NoneValue, [], env')
    evalBound (Just boundExpr) env' = do
      EvalExprResult boundVal boundOuts envNext <- evalExprFn env' fenv boundExpr
      Right (boundVal, boundOuts, envNext)
