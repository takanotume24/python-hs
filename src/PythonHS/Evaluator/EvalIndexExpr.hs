module PythonHS.Evaluator.EvalIndexExpr (evalIndexExpr) where

import PythonHS.Evaluator.EvalExprResult (EvalExprResult (..))
import PythonHS.Evaluator.EvalIndexExprInput (EvalIndexExprInput (..))
import PythonHS.VM.GetitemValue (getitemValue)
import PythonHS.VM.GetitemValueConfig (GetitemValueConfig (..))

evalIndexExpr :: EvalIndexExprInput -> Either String EvalExprResult
evalIndexExpr input = do
  let evalExprFn = evalIndexExprInputEvalExpr input
      fenv = evalIndexExprInputFuncEnv input
      pos = evalIndexExprInputPos input
  EvalExprResult baseVal baseOuts envAfterBase <- evalExprFn (evalIndexExprInputEnv input) fenv (evalIndexExprInputBase input)
  EvalExprResult idxVal idxOuts envAfterIdx <- evalExprFn envAfterBase fenv (evalIndexExprInputIndex input)
  resultVal <- getitemValue GetitemValueConfig {getitemValuePos = pos, getitemValueSeqValue = baseVal, getitemValueIndexValue = idxVal}
  Right (EvalExprResult resultVal (baseOuts ++ idxOuts) envAfterIdx)
