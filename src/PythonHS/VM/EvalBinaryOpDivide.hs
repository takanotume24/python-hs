module PythonHS.VM.EvalBinaryOpDivide (evalBinaryOpDivide) where

import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.EvalBinaryOpDivideConfig (EvalBinaryOpDivideConfig (..))

evalBinaryOpDivide :: EvalBinaryOpDivideConfig -> Either String Value
evalBinaryOpDivide config = do
  leftNumber <- expectNumber pos left
  rightNumber <- expectNumber pos right
  if rightNumber == 0
    then Left ("Value error: division by zero at " ++ showPos pos)
    else Right (FloatValue {floatValue = leftNumber / rightNumber})
  where
    left = evalBinaryOpDivideConfigLeft config
    right = evalBinaryOpDivideConfigRight config
    pos = evalBinaryOpDivideConfigPos config
    expectNumber = evalBinaryOpDivideConfigExpectNumber config
