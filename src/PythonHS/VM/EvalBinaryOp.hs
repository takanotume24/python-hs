module PythonHS.VM.EvalBinaryOp (evalBinaryOp) where

import PythonHS.AST.BinaryOperator (BinaryOperator (..))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (DictValue, FloatValue, IntValue, ListValue, NoneValue, StringValue))
import PythonHS.Lexer.Position (Position)

evalBinaryOp :: BinaryOperator -> Value -> Value -> Position -> Either String Value
evalBinaryOp op left right pos =
  case op of
    AddOperator ->
      case (left, right) of
        (IntValue l, IntValue r) -> Right (IntValue (l + r))
        (FloatValue l, FloatValue r) -> Right (FloatValue (l + r))
        (IntValue l, FloatValue r) -> Right (FloatValue (fromIntegral l + r))
        (FloatValue l, IntValue r) -> Right (FloatValue (l + fromIntegral r))
        (StringValue l, StringValue r) -> Right (StringValue (l ++ r))
        _ -> Left ("Type error: + expects int+int or string+string at " ++ showPos pos)
    SubtractOperator -> evalNumericBinary "-" pos left right (-)
    MultiplyOperator -> evalNumericBinary "*" pos left right (*)
    DivideOperator -> evalDivide pos left right
    FloorDivideOperator -> evalFloorDivide pos left right
    ModuloOperator -> evalModulo pos left right
    EqOperator -> evalEqComparison left right
    NotEqOperator -> evalNotEqComparison left right
    LtOperator -> evalNumericComparison "<" pos left right (<)
    GtOperator -> evalNumericComparison ">" pos left right (>)
    LteOperator -> evalNumericComparison "<=" pos left right (<=)
    GteOperator -> evalNumericComparison ">=" pos left right (>=)
    AndOperator -> do
      leftTruthy <- expectTruthy "and" pos left
      rightTruthy <- expectTruthy "and" pos right
      Right (IntValue (if leftTruthy /= 0 && rightTruthy /= 0 then 1 else 0))
    OrOperator -> do
      leftTruthy <- expectTruthy "or" pos left
      rightTruthy <- expectTruthy "or" pos right
      Right (IntValue (if leftTruthy /= 0 || rightTruthy /= 0 then 1 else 0))
  where
    evalNumericBinary context pos' left' right' opFn = do
      leftNumber <- expectNumber context pos' left'
      rightNumber <- expectNumber context pos' right'
      case (left', right') of
        (IntValue _, IntValue _) -> Right (IntValue (truncate (opFn leftNumber rightNumber)))
        _ -> Right (FloatValue (opFn leftNumber rightNumber))

    evalDivide pos' left' right' = do
      leftNumber <- expectNumber "/" pos' left'
      rightNumber <- expectNumber "/" pos' right'
      if rightNumber == 0
        then Left ("Value error: division by zero at " ++ showPos pos')
        else Right (FloatValue (leftNumber / rightNumber))

    evalFloorDivide pos' left' right' = do
      leftNumber <- expectNumber "//" pos' left'
      rightNumber <- expectNumber "//" pos' right'
      if rightNumber == 0
        then Left ("Value error: division by zero at " ++ showPos pos')
        else
          let quotient = leftNumber / rightNumber
              floored = fromIntegral (floor quotient :: Int)
           in case (left', right') of
                (IntValue _, IntValue _) -> Right (IntValue (floor quotient))
                _ -> Right (FloatValue floored)

    evalModulo pos' left' right' = do
      leftNumber <- expectNumber "%" pos' left'
      rightNumber <- expectNumber "%" pos' right'
      if rightNumber == 0
        then Left ("Value error: modulo by zero at " ++ showPos pos')
        else
          let quotient = leftNumber / rightNumber
              floored = fromIntegral (floor quotient :: Int)
              remainder = leftNumber - rightNumber * floored
           in case (left', right') of
                (IntValue leftInt, IntValue rightInt) -> Right (IntValue (leftInt `mod` rightInt))
                _ -> Right (FloatValue remainder)

    evalEqComparison left' right' =
      case (left', right') of
        (IntValue leftInt, FloatValue rightFloat) -> Right (IntValue (if (fromIntegral leftInt :: Double) == rightFloat then 1 else 0))
        (FloatValue leftFloat, IntValue rightInt) -> Right (IntValue (if leftFloat == (fromIntegral rightInt :: Double) then 1 else 0))
        _ -> Right (IntValue (if left' == right' then 1 else 0))

    evalNotEqComparison left' right' =
      case (left', right') of
        (IntValue leftInt, FloatValue rightFloat) -> Right (IntValue (if (fromIntegral leftInt :: Double) /= rightFloat then 1 else 0))
        (FloatValue leftFloat, IntValue rightInt) -> Right (IntValue (if leftFloat /= (fromIntegral rightInt :: Double) then 1 else 0))
        _ -> Right (IntValue (if left' /= right' then 1 else 0))

    evalNumericComparison context pos' left' right' cmp = do
      leftNumber <- expectNumber context pos' left'
      rightNumber <- expectNumber context pos' right'
      Right (IntValue (if cmp leftNumber rightNumber then 1 else 0))

    expectNumber _ _ (IntValue n) = Right (fromIntegral n)
    expectNumber _ _ (FloatValue n) = Right n
    expectNumber _ _ NoneValue = Right 0
    expectNumber context pos' _ = Left ("Type error: expected int in " ++ context ++ " at " ++ showPos pos')

    expectTruthy :: String -> Position -> Value -> Either String Int
    expectTruthy _ _ (IntValue n) = Right (if n == 0 then 0 else 1)
    expectTruthy _ _ (FloatValue n) = Right (if n == 0 then 0 else 1)
    expectTruthy _ _ NoneValue = Right 0
    expectTruthy _ _ (StringValue s) = Right (if null s then 0 else 1)
    expectTruthy _ _ (ListValue vals) = Right (if null vals then 0 else 1)
    expectTruthy _ _ (DictValue pairs) = Right (if null pairs then 0 else 1)
    expectTruthy context pos' _ = Left ("Type error: expected int in " ++ context ++ " at " ++ showPos pos')
