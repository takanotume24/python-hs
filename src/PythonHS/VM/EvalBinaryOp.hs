module PythonHS.VM.EvalBinaryOp (evalBinaryOp) where

import PythonHS.AST.BinaryOperator (BinaryOperator (..))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.EvalBinaryOpConfig (EvalBinaryOpConfig (..))
import PythonHS.VM.EvalBinaryOpDivide (evalBinaryOpDivide)
import PythonHS.VM.EvalBinaryOpDivideConfig (EvalBinaryOpDivideConfig (..))
import PythonHS.VM.EvalBinaryOpEquality (evalBinaryOpEquality)
import PythonHS.VM.EvalBinaryOpEqualityConfig (EvalBinaryOpEqualityConfig (..))
import PythonHS.VM.EvalBinaryOpIdentity (evalBinaryOpIdentity)
import PythonHS.VM.EvalBinaryOpIdentityConfig (EvalBinaryOpIdentityConfig (..))
import PythonHS.VM.EvalBinaryOpMembership (evalBinaryOpMembership)
import PythonHS.VM.EvalBinaryOpMembershipConfig (EvalBinaryOpMembershipConfig (..))
import PythonHS.VM.EvalBinaryOpPower (evalBinaryOpPower)
import PythonHS.VM.EvalBinaryOpPowerConfig (EvalBinaryOpPowerConfig (..))

evalBinaryOp :: EvalBinaryOpConfig -> Either String Value
evalBinaryOp config =
  let op = evalBinaryOpOp config
      left = evalBinaryOpLeft config
      right = evalBinaryOpRight config
      pos = evalBinaryOpPos config
   in case op of
        AddOperator ->
          case left of
            IntValue {intValue = l} ->
              case right of
                IntValue {intValue = r} -> Right (IntValue {intValue = l + r})
                FloatValue {floatValue = r} -> Right (FloatValue {floatValue = fromIntegral l + r})
                _ -> Left ("Type error: + expects int+int or string+string at " ++ showPos pos)
            FloatValue {floatValue = l} ->
              case right of
                FloatValue {floatValue = r} -> Right (FloatValue {floatValue = l + r})
                IntValue {intValue = r} -> Right (FloatValue {floatValue = l + fromIntegral r})
                _ -> Left ("Type error: + expects int+int or string+string at " ++ showPos pos)
            StringValue {stringValue = l} ->
              case right of
                StringValue {stringValue = r} -> Right (StringValue {stringValue = l ++ r})
                _ -> Left ("Type error: + expects int+int or string+string at " ++ showPos pos)
            _ -> Left ("Type error: + expects int+int or string+string at " ++ showPos pos)
        SubtractOperator -> evalNumericBinary "-" pos left right (-)
        MultiplyOperator -> evalNumericBinary "*" pos left right (*)
        DivideOperator -> evalBinaryOpDivide EvalBinaryOpDivideConfig {evalBinaryOpDivideConfigLeft = left, evalBinaryOpDivideConfigRight = right, evalBinaryOpDivideConfigPos = pos, evalBinaryOpDivideConfigExpectNumber = expectNumber "/"}
        FloorDivideOperator -> evalFloorDivide pos left right
        ModuloOperator -> evalModulo pos left right
        PowerOperator -> evalBinaryOpPower EvalBinaryOpPowerConfig {evalBinaryOpPowerConfigLeft = left, evalBinaryOpPowerConfigRight = right, evalBinaryOpPowerConfigPos = pos, evalBinaryOpPowerConfigExpectNumber = expectNumber "**"}
        EqOperator -> evalBinaryOpEquality EvalBinaryOpEqualityConfig {evalBinaryOpEqualityConfigLeft = left, evalBinaryOpEqualityConfigRight = right, evalBinaryOpEqualityConfigNegate = False}
        NotEqOperator -> evalBinaryOpEquality EvalBinaryOpEqualityConfig {evalBinaryOpEqualityConfigLeft = left, evalBinaryOpEqualityConfigRight = right, evalBinaryOpEqualityConfigNegate = True}
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
        InOperator -> evalBinaryOpMembership EvalBinaryOpMembershipConfig {evalBinaryOpMembershipConfigLeft = left, evalBinaryOpMembershipConfigRight = right, evalBinaryOpMembershipConfigPos = pos, evalBinaryOpMembershipConfigNegate = False}
        NotInOperator -> evalBinaryOpMembership EvalBinaryOpMembershipConfig {evalBinaryOpMembershipConfigLeft = left, evalBinaryOpMembershipConfigRight = right, evalBinaryOpMembershipConfigPos = pos, evalBinaryOpMembershipConfigNegate = True}
        IsOperator -> evalBinaryOpIdentity EvalBinaryOpIdentityConfig {evalBinaryOpIdentityConfigLeft = left, evalBinaryOpIdentityConfigRight = right, evalBinaryOpIdentityConfigPos = pos, evalBinaryOpIdentityConfigNegate = False}
        IsNotOperator -> evalBinaryOpIdentity EvalBinaryOpIdentityConfig {evalBinaryOpIdentityConfigLeft = left, evalBinaryOpIdentityConfigRight = right, evalBinaryOpIdentityConfigPos = pos, evalBinaryOpIdentityConfigNegate = True}
  where
    evalNumericBinary context pos' left' right' opFn = do
      leftNumber <- expectNumber context pos' left'
      rightNumber <- expectNumber context pos' right'
      case left' of
        IntValue {intValue = _} ->
          case right' of
            IntValue {intValue = _} -> Right (IntValue {intValue = truncate (opFn leftNumber rightNumber)})
            _ -> Right (FloatValue {floatValue = opFn leftNumber rightNumber})
        _ -> Right (FloatValue {floatValue = opFn leftNumber rightNumber})

    evalFloorDivide pos' left' right' = do
      leftNumber <- expectNumber "//" pos' left'
      rightNumber <- expectNumber "//" pos' right'
      if rightNumber == 0
        then Left ("Value error: division by zero at " ++ showPos pos')
        else
          let quotient = leftNumber / rightNumber
              floored = fromIntegral (floor quotient :: Int)
           in case left' of
                IntValue {intValue = _} ->
                  case right' of
                    IntValue {intValue = _} -> Right (IntValue {intValue = floor quotient})
                    _ -> Right (FloatValue {floatValue = floored})
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
           in case left' of
                IntValue {intValue = leftInt} ->
                  case right' of
                    IntValue {intValue = rightInt} -> Right (IntValue {intValue = leftInt `mod` rightInt})
                    _ -> Right (FloatValue {floatValue = remainder})
                _ -> Right (FloatValue {floatValue = remainder})

    evalNumericComparison context pos' left' right' cmp = do
      leftNumber <- expectNumber context pos' left'
      rightNumber <- expectNumber context pos' right'
      Right (IntValue {intValue = if cmp leftNumber rightNumber then 1 else 0})

    evalOrderComparison context pos' left' right' cmp =
      let fallback = evalNumericComparison context pos' left' right' (\l r -> cmp (compare l r))
       in case left' of
            InstanceValue {instanceValueClass = leftClass, instanceValueAttrs = leftAttrs} ->
              case right' of
                InstanceValue {instanceValueClass = rightClass, instanceValueAttrs = rightAttrs} ->
                  if leftClass == rightClass
                    then do
                      ordResult <- compareInstanceValues leftAttrs rightAttrs
                      Right (IntValue {intValue = if cmp ordResult then 1 else 0})
                    else Left ("Type error: expected int in " ++ context ++ " at " ++ showPos pos')
                _ -> fallback
            TupleValue {tupleValueItems = leftVals} ->
              case right' of
                TupleValue {tupleValueItems = rightVals} -> do
                  ordResult <- compareLists leftVals rightVals
                  Right (IntValue {intValue = if cmp ordResult then 1 else 0})
                _ -> fallback
            _ -> fallback

    compareInstanceValues leftAttrs rightAttrs =
      compareLists (filterDataclassAttrs leftAttrs) (filterDataclassAttrs rightAttrs)

    filterDataclassAttrs [] = []
    filterDataclassAttrs (pair : rest) =
      if fst pair == "__python_hs_frozen__"
        then filterDataclassAttrs rest
        else snd pair : filterDataclassAttrs rest

    compareLists [] [] = Right EQ
    compareLists [] (_ : _) = Right LT
    compareLists (_ : _) [] = Right GT
    compareLists (leftValue : leftRest) (rightValue : rightRest) =
      let firstCompare = compareSingleValue leftValue rightValue
       in if firstCompare == EQ
            then compareLists leftRest rightRest
            else Right firstCompare

    compareSingleValue (IntValue {intValue = leftInt}) (IntValue {intValue = rightInt}) = compare leftInt rightInt
    compareSingleValue (FloatValue {floatValue = leftFloat}) (FloatValue {floatValue = rightFloat}) = compare leftFloat rightFloat
    compareSingleValue (IntValue {intValue = leftInt}) (FloatValue {floatValue = rightFloat}) = compare (fromIntegral leftInt :: Double) rightFloat
    compareSingleValue (FloatValue {floatValue = leftFloat}) (IntValue {intValue = rightInt}) = compare leftFloat (fromIntegral rightInt :: Double)
    compareSingleValue (StringValue {stringValue = leftString}) (StringValue {stringValue = rightString}) = compare leftString rightString
    compareSingleValue leftValue rightValue = compare (show leftValue) (show rightValue)

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
