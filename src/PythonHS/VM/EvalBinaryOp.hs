module PythonHS.VM.EvalBinaryOp (evalBinaryOp) where

import PythonHS.AST.BinaryOperator (BinaryOperator (..))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.EvalBinaryOpConfig (EvalBinaryOpConfig (..))

evalBinaryOp :: EvalBinaryOpConfig -> Either String Value
evalBinaryOp config =
  let op = evalBinaryOpOp config
      left = evalBinaryOpLeft config
      right = evalBinaryOpRight config
      pos = evalBinaryOpPos config
   in case op of
        AddOperator ->
          case (left, right) of
            (IntValue {intValue = l}, IntValue {intValue = r}) -> Right (IntValue {intValue = l + r})
            (FloatValue {floatValue = l}, FloatValue {floatValue = r}) -> Right (FloatValue {floatValue = l + r})
            (IntValue {intValue = l}, FloatValue {floatValue = r}) -> Right (FloatValue {floatValue = fromIntegral l + r})
            (FloatValue {floatValue = l}, IntValue {intValue = r}) -> Right (FloatValue {floatValue = l + fromIntegral r})
            (StringValue {stringValue = l}, StringValue {stringValue = r}) -> Right (StringValue {stringValue = l ++ r})
            _ -> Left ("Type error: + expects int+int or string+string at " ++ showPos pos)
        SubtractOperator -> evalNumericBinary "-" pos left right (-)
        MultiplyOperator -> evalNumericBinary "*" pos left right (*)
        DivideOperator -> evalDivide pos left right
        FloorDivideOperator -> evalFloorDivide pos left right
        ModuloOperator -> evalModulo pos left right
        EqOperator -> evalEqComparison left right
        NotEqOperator -> evalNotEqComparison left right
        LtOperator -> evalOrderComparison "<" pos left right (\ordResult -> ordResult == LT)
        GtOperator -> evalOrderComparison ">" pos left right (\ordResult -> ordResult == GT)
        LteOperator -> evalOrderComparison "<=" pos left right (\ordResult -> ordResult /= GT)
        GteOperator -> evalOrderComparison ">=" pos left right (\ordResult -> ordResult /= LT)
        AndOperator -> do
          leftTruthy <- expectTruthy "and" pos left
          rightTruthy <- expectTruthy "and" pos right
          Right (IntValue {intValue = if leftTruthy /= 0 && rightTruthy /= 0 then 1 else 0})
        OrOperator -> do
          leftTruthy <- expectTruthy "or" pos left
          rightTruthy <- expectTruthy "or" pos right
          Right (IntValue {intValue = if leftTruthy /= 0 || rightTruthy /= 0 then 1 else 0})
  where
    evalNumericBinary context pos' left' right' opFn = do
      leftNumber <- expectNumber context pos' left'
      rightNumber <- expectNumber context pos' right'
      case (left', right') of
        (IntValue {intValue = _}, IntValue {intValue = _}) -> Right (IntValue {intValue = truncate (opFn leftNumber rightNumber)})
        _ -> Right (FloatValue {floatValue = opFn leftNumber rightNumber})

    evalDivide pos' left' right' = do
      leftNumber <- expectNumber "/" pos' left'
      rightNumber <- expectNumber "/" pos' right'
      if rightNumber == 0
        then Left ("Value error: division by zero at " ++ showPos pos')
        else Right (FloatValue {floatValue = leftNumber / rightNumber})

    evalFloorDivide pos' left' right' = do
      leftNumber <- expectNumber "//" pos' left'
      rightNumber <- expectNumber "//" pos' right'
      if rightNumber == 0
        then Left ("Value error: division by zero at " ++ showPos pos')
        else
          let quotient = leftNumber / rightNumber
              floored = fromIntegral (floor quotient :: Int)
           in case (left', right') of
                (IntValue {intValue = _}, IntValue {intValue = _}) -> Right (IntValue {intValue = floor quotient})
                _ -> Right (FloatValue {floatValue = floored})

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
                (IntValue {intValue = leftInt}, IntValue {intValue = rightInt}) -> Right (IntValue {intValue = leftInt `mod` rightInt})
                _ -> Right (FloatValue {floatValue = remainder})

    evalEqComparison left' right' =
      case (left', right') of
        (IntValue {intValue = leftInt}, FloatValue {floatValue = rightFloat}) -> Right (IntValue {intValue = if (fromIntegral leftInt :: Double) == rightFloat then 1 else 0})
        (FloatValue {floatValue = leftFloat}, IntValue {intValue = rightInt}) -> Right (IntValue {intValue = if leftFloat == (fromIntegral rightInt :: Double) then 1 else 0})
        (TupleValue {tupleValueItems = leftVals}, TupleValue {tupleValueItems = rightVals}) -> Right (IntValue {intValue = if leftVals == rightVals then 1 else 0})
        _ -> Right (IntValue {intValue = if left' == right' then 1 else 0})

    evalNotEqComparison left' right' =
      case (left', right') of
        (IntValue {intValue = leftInt}, FloatValue {floatValue = rightFloat}) -> Right (IntValue {intValue = if (fromIntegral leftInt :: Double) /= rightFloat then 1 else 0})
        (FloatValue {floatValue = leftFloat}, IntValue {intValue = rightInt}) -> Right (IntValue {intValue = if leftFloat /= (fromIntegral rightInt :: Double) then 1 else 0})
        (TupleValue {tupleValueItems = leftVals}, TupleValue {tupleValueItems = rightVals}) -> Right (IntValue {intValue = if leftVals /= rightVals then 1 else 0})
        _ -> Right (IntValue {intValue = if left' /= right' then 1 else 0})

    evalNumericComparison context pos' left' right' cmp = do
      leftNumber <- expectNumber context pos' left'
      rightNumber <- expectNumber context pos' right'
      Right (IntValue {intValue = if cmp leftNumber rightNumber then 1 else 0})

    evalOrderComparison context pos' left' right' cmp =
      case (left', right') of
        (InstanceValue {instanceValueClass = leftClass, instanceValueAttrs = leftAttrs}, InstanceValue {instanceValueClass = rightClass, instanceValueAttrs = rightAttrs}) ->
          if leftClass == rightClass
            then do
              ordResult <- compareInstanceValues leftAttrs rightAttrs
              Right (IntValue {intValue = if cmp ordResult then 1 else 0})
            else Left ("Type error: expected int in " ++ context ++ " at " ++ showPos pos')
        (TupleValue {tupleValueItems = leftVals}, TupleValue {tupleValueItems = rightVals}) -> do
          ordResult <- compareLists leftVals rightVals
          Right (IntValue {intValue = if cmp ordResult then 1 else 0})
        _ -> evalNumericComparison context pos' left' right' (\l r -> cmp (compare l r))

    compareInstanceValues leftAttrs rightAttrs =
      compareLists (filterDataclassAttrs leftAttrs) (filterDataclassAttrs rightAttrs)

    filterDataclassAttrs pairs =
      case pairs of
        [] -> []
        (name, value) : rest ->
          if name == "__python_hs_frozen__"
            then filterDataclassAttrs rest
            else value : filterDataclassAttrs rest

    compareLists leftValues rightValues =
      case (leftValues, rightValues) of
        ([], []) -> Right EQ
        ([], _ : _) -> Right LT
        (_ : _, []) -> Right GT
        (leftValue : leftRest, rightValue : rightRest) ->
          let firstCompare = compareSingleValue leftValue rightValue
           in if firstCompare == EQ
                then compareLists leftRest rightRest
                else Right firstCompare

    compareSingleValue leftValue rightValue =
      case (leftValue, rightValue) of
        (IntValue {intValue = leftInt}, IntValue {intValue = rightInt}) -> compare leftInt rightInt
        (FloatValue {floatValue = leftFloat}, FloatValue {floatValue = rightFloat}) -> compare leftFloat rightFloat
        (IntValue {intValue = leftInt}, FloatValue {floatValue = rightFloat}) -> compare (fromIntegral leftInt :: Double) rightFloat
        (FloatValue {floatValue = leftFloat}, IntValue {intValue = rightInt}) -> compare leftFloat (fromIntegral rightInt :: Double)
        (StringValue {stringValue = leftString}, StringValue {stringValue = rightString}) -> compare leftString rightString
        _ -> compare (show leftValue) (show rightValue)

    expectNumber _ _ IntValue {intValue = n} = Right (fromIntegral n)
    expectNumber _ _ FloatValue {floatValue = n} = Right n
    expectNumber _ _ NoneValue = Right 0
    expectNumber context pos' _ = Left ("Type error: expected int in " ++ context ++ " at " ++ showPos pos')

    expectTruthy :: String -> Position -> Value -> Either String Int
    expectTruthy _ _ IntValue {intValue = n} = Right (if n == 0 then 0 else 1)
    expectTruthy _ _ FloatValue {floatValue = n} = Right (if n == 0 then 0 else 1)
    expectTruthy _ _ NoneValue = Right 0
    expectTruthy _ _ StringValue {stringValue = s} = Right (if null s then 0 else 1)
    expectTruthy _ _ ListValue {listValueItems = vals} = Right (if null vals then 0 else 1)
    expectTruthy _ _ DictValue {dictValuePairs = pairs} = Right (if null pairs then 0 else 1)
    expectTruthy context pos' _ = Left ("Type error: expected int in " ++ context ++ " at " ++ showPos pos')
