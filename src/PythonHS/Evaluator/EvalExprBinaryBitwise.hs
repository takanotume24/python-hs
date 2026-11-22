module PythonHS.Evaluator.EvalExprBinaryBitwise (evalExprBinaryBitwise) where

import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import PythonHS.AST.BinaryOperator (BinaryOperator (BitwiseAndOperator, BitwiseOrOperator, BitwiseXorOperator, LeftShiftOperator, RightShiftOperator))
import PythonHS.Evaluator.EvalExprBinaryBitwiseConfig (EvalExprBinaryBitwiseConfig (..))
import PythonHS.Evaluator.EvalExprResult (EvalExprResult (..))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))

evalExprBinaryBitwise :: EvalExprBinaryBitwiseConfig -> Either String EvalExprResult
evalExprBinaryBitwise config = do
  let evalExprFn = evalExprBinaryBitwiseConfigEvalExprFn config
      env = evalExprBinaryBitwiseConfigEnv config
      fenv = evalExprBinaryBitwiseConfigFuncEnv config
      leftExpr = evalExprBinaryBitwiseConfigLeftExpr config
      rightExpr = evalExprBinaryBitwiseConfigRightExpr config
      op = evalExprBinaryBitwiseConfigOp config
      pos = evalExprBinaryBitwiseConfigPos config
      opName = operatorName op
  EvalExprResult {evalExprResultValue = leftVal, evalExprResultOutputs = leftOuts, evalExprResultEnv = envAfterLeft} <- evalExprFn env fenv leftExpr
  EvalExprResult {evalExprResultValue = rightVal, evalExprResultOutputs = rightOuts, evalExprResultEnv = envAfterRight} <- evalExprFn envAfterLeft fenv rightExpr
  result <- evalBitwiseValue op opName pos leftVal rightVal
  Right (EvalExprResult {evalExprResultValue = result, evalExprResultOutputs = leftOuts ++ rightOuts, evalExprResultEnv = envAfterRight})
  where
    evalBitwiseValue BitwiseAndOperator _ _ IntValue {intValue = leftInt} IntValue {intValue = rightInt} = Right (IntValue {intValue = leftInt .&. rightInt})
    evalBitwiseValue BitwiseOrOperator _ _ IntValue {intValue = leftInt} IntValue {intValue = rightInt} = Right (IntValue {intValue = leftInt .|. rightInt})
    evalBitwiseValue BitwiseXorOperator _ _ IntValue {intValue = leftInt} IntValue {intValue = rightInt} = Right (IntValue {intValue = leftInt `xor` rightInt})
    evalBitwiseValue LeftShiftOperator _ pos' IntValue {intValue = leftInt} IntValue {intValue = rightInt} =
      if rightInt < 0
        then Left ("Value error: negative shift count at " ++ showPos pos')
        else Right (IntValue {intValue = leftInt `shiftL` fromIntegral rightInt})
    evalBitwiseValue RightShiftOperator _ pos' IntValue {intValue = leftInt} IntValue {intValue = rightInt} =
      if rightInt < 0
        then Left ("Value error: negative shift count at " ++ showPos pos')
        else Right (IntValue {intValue = leftInt `shiftR` fromIntegral rightInt})
    evalBitwiseValue _ opName pos' _ _ = Left ("Type error: " ++ opName ++ " expects int at " ++ showPos pos')

    operatorName BitwiseAndOperator = "&"
    operatorName BitwiseOrOperator = "|"
    operatorName BitwiseXorOperator = "^"
    operatorName LeftShiftOperator = "<<"
    operatorName RightShiftOperator = ">>"
    operatorName _ = "bitwise op"
