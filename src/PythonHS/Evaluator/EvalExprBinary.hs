module PythonHS.Evaluator.EvalExprBinary (evalExprBinary) where

import PythonHS.AST.BinaryOperator (BinaryOperator (..))
import PythonHS.AST.Expr (Expr)
import PythonHS.Evaluator.EvalExprBinaryInput (EvalExprBinaryInput (..))
import PythonHS.Evaluator.EvalExprResult (EvalExprResult (..))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Lexer.Position (Position)

evalExprBinary :: EvalExprBinaryInput -> Either String EvalExprResult
evalExprBinary input = case op of
  AddOperator -> do
    EvalExprResult {evalExprResultValue = leftVal, evalExprResultOutputs = leftOuts, evalExprResultEnv = envAfterLeft} <- evalExprFn env fenv leftExpr
    EvalExprResult {evalExprResultValue = rightVal, evalExprResultOutputs = rightOuts, evalExprResultEnv = envAfterRight} <- evalExprFn envAfterLeft fenv rightExpr
    case (leftVal, rightVal) of
      (IntValue {intValue = leftInt}, IntValue {intValue = rightInt}) -> Right (EvalExprResult {evalExprResultValue = IntValue {intValue = leftInt + rightInt}, evalExprResultOutputs = leftOuts ++ rightOuts, evalExprResultEnv = envAfterRight})
      (FloatValue {floatValue = leftFloat}, FloatValue {floatValue = rightFloat}) -> Right (EvalExprResult {evalExprResultValue = FloatValue {floatValue = leftFloat + rightFloat}, evalExprResultOutputs = leftOuts ++ rightOuts, evalExprResultEnv = envAfterRight})
      (IntValue {intValue = leftInt}, FloatValue {floatValue = rightFloat}) -> Right (EvalExprResult {evalExprResultValue = FloatValue {floatValue = fromIntegral leftInt + rightFloat}, evalExprResultOutputs = leftOuts ++ rightOuts, evalExprResultEnv = envAfterRight})
      (FloatValue {floatValue = leftFloat}, IntValue {intValue = rightInt}) -> Right (EvalExprResult {evalExprResultValue = FloatValue {floatValue = leftFloat + fromIntegral rightInt}, evalExprResultOutputs = leftOuts ++ rightOuts, evalExprResultEnv = envAfterRight})
      (StringValue {stringValue = leftString}, StringValue {stringValue = rightString}) -> Right (EvalExprResult {evalExprResultValue = StringValue {stringValue = leftString ++ rightString}, evalExprResultOutputs = leftOuts ++ rightOuts, evalExprResultEnv = envAfterRight})
      _ -> Left $ "Type error: + expects int+int or string+string at " ++ showPos pos
  SubtractOperator -> evalNumericBinary env fenv leftExpr rightExpr pos (\leftNumber rightNumber -> leftNumber - rightNumber)
  MultiplyOperator -> evalNumericBinary env fenv leftExpr rightExpr pos (\leftNumber rightNumber -> leftNumber * rightNumber)
  DivideOperator -> evalDivide env fenv leftExpr rightExpr pos
  FloorDivideOperator -> evalFloorDivide env fenv leftExpr rightExpr pos
  ModuloOperator -> evalModulo env fenv leftExpr rightExpr pos
  EqOperator -> evalEqComparison env fenv leftExpr rightExpr pos
  NotEqOperator -> evalNotEqComparison env fenv leftExpr rightExpr pos
  LtOperator -> evalNumericComparison env fenv leftExpr rightExpr pos "<" (<)
  GtOperator -> evalNumericComparison env fenv leftExpr rightExpr pos ">" (>)
  LteOperator -> evalNumericComparison env fenv leftExpr rightExpr pos "<=" (<=)
  GteOperator -> evalNumericComparison env fenv leftExpr rightExpr pos ">=" (>=)
  AndOperator -> do
    EvalExprResult {evalExprResultValue = leftVal} <- evalExprFn env fenv leftExpr
    EvalExprResult {evalExprResultOutputs = leftOuts, evalExprResultEnv = envAfterLeft} <- evalExprFn env fenv leftExpr
    leftTruthy <- expectTruthy "and" pos leftVal
    if leftTruthy == 0
      then Right (EvalExprResult {evalExprResultValue = IntValue {intValue = 0}, evalExprResultOutputs = leftOuts, evalExprResultEnv = envAfterLeft})
      else do
        EvalExprResult {evalExprResultValue = rightVal, evalExprResultOutputs = rightOuts, evalExprResultEnv = envAfterRight} <- evalExprFn envAfterLeft fenv rightExpr
        rightTruthy <- expectTruthy "and" pos rightVal
        Right (EvalExprResult {evalExprResultValue = IntValue {intValue = if rightTruthy /= 0 then 1 else 0}, evalExprResultOutputs = leftOuts ++ rightOuts, evalExprResultEnv = envAfterRight})
  OrOperator -> do
    EvalExprResult {evalExprResultValue = leftVal} <- evalExprFn env fenv leftExpr
    EvalExprResult {evalExprResultOutputs = leftOuts, evalExprResultEnv = envAfterLeft} <- evalExprFn env fenv leftExpr
    leftTruthy <- expectTruthy "or" pos leftVal
    if leftTruthy /= 0
      then Right (EvalExprResult {evalExprResultValue = IntValue {intValue = 1}, evalExprResultOutputs = leftOuts, evalExprResultEnv = envAfterLeft})
      else do
        EvalExprResult {evalExprResultValue = rightVal, evalExprResultOutputs = rightOuts, evalExprResultEnv = envAfterRight} <- evalExprFn envAfterLeft fenv rightExpr
        rightTruthy <- expectTruthy "or" pos rightVal
        Right (EvalExprResult {evalExprResultValue = IntValue {intValue = if rightTruthy /= 0 then 1 else 0}, evalExprResultOutputs = leftOuts ++ rightOuts, evalExprResultEnv = envAfterRight})
  where
    evalExprFn = evalExprBinaryEvalExprFn input
    env = evalExprBinaryEnv input
    fenv = evalExprBinaryFuncEnv input
    op = evalExprBinaryOp input
    leftExpr = evalExprBinaryLeftExpr input
    rightExpr = evalExprBinaryRightExpr input
    pos = evalExprBinaryPos input

    evalNumericBinary currentEnv currentFenv le re pos' opFn = do
      EvalExprResult {evalExprResultValue = leftVal, evalExprResultOutputs = leftOuts, evalExprResultEnv = envAfterLeft} <- evalExprFn currentEnv currentFenv le
      EvalExprResult {evalExprResultValue = rightVal, evalExprResultOutputs = rightOuts, evalExprResultEnv = envAfterRight} <- evalExprFn envAfterLeft currentFenv re
      leftNumber <- expectNumber context pos' leftVal
      rightNumber <- expectNumber context pos' rightVal
      case (leftVal, rightVal) of
        (IntValue {}, IntValue {}) -> Right (EvalExprResult {evalExprResultValue = IntValue {intValue = truncate (opFn leftNumber rightNumber)}, evalExprResultOutputs = leftOuts ++ rightOuts, evalExprResultEnv = envAfterRight})
        _ -> Right (EvalExprResult {evalExprResultValue = FloatValue {floatValue = opFn leftNumber rightNumber}, evalExprResultOutputs = leftOuts ++ rightOuts, evalExprResultEnv = envAfterRight})
      where
        context = case op of SubtractOperator -> "-"; MultiplyOperator -> "*"; _ -> ""

    evalDivide currentEnv currentFenv le re pos' = do
      EvalExprResult {evalExprResultValue = leftVal, evalExprResultOutputs = leftOuts, evalExprResultEnv = envAfterLeft} <- evalExprFn currentEnv currentFenv le
      EvalExprResult {evalExprResultValue = rightVal, evalExprResultOutputs = rightOuts, evalExprResultEnv = envAfterRight} <- evalExprFn envAfterLeft currentFenv re
      leftNumber <- expectNumber "/" pos' leftVal
      rightNumber <- expectNumber "/" pos' rightVal
      if rightNumber == 0
        then Left $ "Value error: division by zero at " ++ showPos pos'
        else Right (EvalExprResult {evalExprResultValue = FloatValue {floatValue = leftNumber / rightNumber}, evalExprResultOutputs = leftOuts ++ rightOuts, evalExprResultEnv = envAfterRight})

    evalFloorDivide currentEnv currentFenv le re pos' = do
      EvalExprResult {evalExprResultValue = leftVal, evalExprResultOutputs = leftOuts, evalExprResultEnv = envAfterLeft} <- evalExprFn currentEnv currentFenv le
      EvalExprResult {evalExprResultValue = rightVal, evalExprResultOutputs = rightOuts, evalExprResultEnv = envAfterRight} <- evalExprFn envAfterLeft currentFenv re
      leftNumber <- expectNumber "//" pos' leftVal
      rightNumber <- expectNumber "//" pos' rightVal
      if rightNumber == 0
        then Left $ "Value error: division by zero at " ++ showPos pos'
        else
          let quotient = leftNumber / rightNumber
              floored = fromIntegral (floor quotient :: Int)
           in case (leftVal, rightVal) of
                (IntValue {}, IntValue {}) -> Right (EvalExprResult {evalExprResultValue = IntValue {intValue = floor quotient}, evalExprResultOutputs = leftOuts ++ rightOuts, evalExprResultEnv = envAfterRight})
                _ -> Right (EvalExprResult {evalExprResultValue = FloatValue {floatValue = floored}, evalExprResultOutputs = leftOuts ++ rightOuts, evalExprResultEnv = envAfterRight})

    evalModulo currentEnv currentFenv le re pos' = do
      EvalExprResult {evalExprResultValue = leftVal, evalExprResultOutputs = leftOuts, evalExprResultEnv = envAfterLeft} <- evalExprFn currentEnv currentFenv le
      EvalExprResult {evalExprResultValue = rightVal, evalExprResultOutputs = rightOuts, evalExprResultEnv = envAfterRight} <- evalExprFn envAfterLeft currentFenv re
      leftNumber <- expectNumber "%" pos' leftVal
      rightNumber <- expectNumber "%" pos' rightVal
      if rightNumber == 0
        then Left $ "Value error: modulo by zero at " ++ showPos pos'
        else
          let quotient = leftNumber / rightNumber
              floored = fromIntegral (floor quotient :: Int)
              remainder = leftNumber - rightNumber * floored
           in case (leftVal, rightVal) of
                (IntValue {intValue = leftInt}, IntValue {intValue = rightInt}) -> Right (EvalExprResult {evalExprResultValue = IntValue {intValue = leftInt `mod` rightInt}, evalExprResultOutputs = leftOuts ++ rightOuts, evalExprResultEnv = envAfterRight})
                _ -> Right (EvalExprResult {evalExprResultValue = FloatValue {floatValue = remainder}, evalExprResultOutputs = leftOuts ++ rightOuts, evalExprResultEnv = envAfterRight})

    evalEqComparison currentEnv currentFenv le re pos' = do
      EvalExprResult {evalExprResultValue = leftVal, evalExprResultOutputs = leftOuts, evalExprResultEnv = envAfterLeft} <- evalExprFn currentEnv currentFenv le
      EvalExprResult {evalExprResultValue = rightVal, evalExprResultOutputs = rightOuts, evalExprResultEnv = envAfterRight} <- evalExprFn envAfterLeft currentFenv re
      case (leftVal, rightVal) of
        (IntValue {intValue = leftInt}, FloatValue {floatValue = rightFloat}) -> Right (EvalExprResult {evalExprResultValue = IntValue {intValue = if (fromIntegral leftInt :: Double) == rightFloat then 1 else 0}, evalExprResultOutputs = leftOuts ++ rightOuts, evalExprResultEnv = envAfterRight})
        (FloatValue {floatValue = leftFloat}, IntValue {intValue = rightInt}) -> Right (EvalExprResult {evalExprResultValue = IntValue {intValue = if leftFloat == (fromIntegral rightInt :: Double) then 1 else 0}, evalExprResultOutputs = leftOuts ++ rightOuts, evalExprResultEnv = envAfterRight})
        _ -> Right (EvalExprResult {evalExprResultValue = IntValue {intValue = if leftVal == rightVal then 1 else 0}, evalExprResultOutputs = leftOuts ++ rightOuts, evalExprResultEnv = envAfterRight})

    evalNotEqComparison currentEnv currentFenv le re pos' = do
      EvalExprResult {evalExprResultValue = leftVal, evalExprResultOutputs = leftOuts, evalExprResultEnv = envAfterLeft} <- evalExprFn currentEnv currentFenv le
      EvalExprResult {evalExprResultValue = rightVal, evalExprResultOutputs = rightOuts, evalExprResultEnv = envAfterRight} <- evalExprFn envAfterLeft currentFenv re
      case (leftVal, rightVal) of
        (IntValue {intValue = leftInt}, FloatValue {floatValue = rightFloat}) -> Right (EvalExprResult {evalExprResultValue = IntValue {intValue = if (fromIntegral leftInt :: Double) /= rightFloat then 1 else 0}, evalExprResultOutputs = leftOuts ++ rightOuts, evalExprResultEnv = envAfterRight})
        (FloatValue {floatValue = leftFloat}, IntValue {intValue = rightInt}) -> Right (EvalExprResult {evalExprResultValue = IntValue {intValue = if leftFloat /= (fromIntegral rightInt :: Double) then 1 else 0}, evalExprResultOutputs = leftOuts ++ rightOuts, evalExprResultEnv = envAfterRight})
        _ -> Right (EvalExprResult {evalExprResultValue = IntValue {intValue = if leftVal /= rightVal then 1 else 0}, evalExprResultOutputs = leftOuts ++ rightOuts, evalExprResultEnv = envAfterRight})

    evalNumericComparison currentEnv currentFenv le re pos' context cmp = do
      EvalExprResult {evalExprResultValue = leftVal, evalExprResultOutputs = leftOuts, evalExprResultEnv = envAfterLeft} <- evalExprFn currentEnv currentFenv le
      EvalExprResult {evalExprResultValue = rightVal, evalExprResultOutputs = rightOuts, evalExprResultEnv = envAfterRight} <- evalExprFn envAfterLeft currentFenv re
      leftNumber <- expectNumber context pos' leftVal
      rightNumber <- expectNumber context pos' rightVal
      Right (EvalExprResult {evalExprResultValue = IntValue {intValue = if cmp leftNumber rightNumber then 1 else 0}, evalExprResultOutputs = leftOuts ++ rightOuts, evalExprResultEnv = envAfterRight})

    expectNumber _ _ IntValue {intValue = n} = Right (fromIntegral n)
    expectNumber _ _ FloatValue {floatValue = n} = Right n
    expectNumber _ _ NoneValue = Right 0
    expectNumber context' pos' _ = Left $ "Type error: expected int in " ++ context' ++ " at " ++ showPos pos'

    expectTruthy :: String -> Position -> Value -> Either String Int
    expectTruthy _ _ IntValue {intValue = n} = Right (if n == 0 then 0 else 1)
    expectTruthy _ _ FloatValue {floatValue = n} = Right (if n == 0 then 0 else 1)
    expectTruthy _ _ NoneValue = Right 0
    expectTruthy _ _ StringValue {stringValue = s} = Right (if null s then 0 else 1)
    expectTruthy _ _ ListValue {listValueItems = vals} = Right (if null vals then 0 else 1)
    expectTruthy _ _ TupleValue {tupleValueItems = vals} = Right (if null vals then 0 else 1)
    expectTruthy _ _ DictValue {dictValuePairs = pairs} = Right (if null pairs then 0 else 1)
    expectTruthy context' pos' _ = Left $ "Type error: expected int in " ++ context' ++ " at " ++ showPos pos'
