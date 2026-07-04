module PythonHS.Evaluator.EvalExprBinary (evalExprBinary) where

import PythonHS.AST.BinaryOperator (BinaryOperator (..))
import PythonHS.AST.Expr (Expr)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalExprResult (EvalExprResult (..))
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Lexer.Position (Position)

evalExprBinary ::
  (Env -> FuncEnv -> Expr -> Either String EvalExprResult) ->
  Env ->
  FuncEnv ->
  BinaryOperator ->
  Expr ->
  Expr ->
  Position ->
  Either String EvalExprResult
evalExprBinary evalExprFn env fenv op leftExpr rightExpr pos =
  case op of
    AddOperator -> do
      EvalExprResult leftVal leftOuts envAfterLeft <- evalExprFn env fenv leftExpr
      EvalExprResult rightVal rightOuts envAfterRight <- evalExprFn envAfterLeft fenv rightExpr
      case (leftVal, rightVal) of
        (IntValue {intValue = leftInt}, IntValue {intValue = rightInt}) -> Right (EvalExprResult (IntValue (leftInt + rightInt)) (leftOuts ++ rightOuts) envAfterRight)
        (FloatValue {floatValue = leftFloat}, FloatValue {floatValue = rightFloat}) -> Right (EvalExprResult (FloatValue (leftFloat + rightFloat)) (leftOuts ++ rightOuts) envAfterRight)
        (IntValue {intValue = leftInt}, FloatValue {floatValue = rightFloat}) -> Right (EvalExprResult (FloatValue (fromIntegral leftInt + rightFloat)) (leftOuts ++ rightOuts) envAfterRight)
        (FloatValue {floatValue = leftFloat}, IntValue {intValue = rightInt}) -> Right (EvalExprResult (FloatValue (leftFloat + fromIntegral rightInt)) (leftOuts ++ rightOuts) envAfterRight)
        (StringValue {stringValue = leftString}, StringValue {stringValue = rightString}) -> Right (EvalExprResult (StringValue (leftString ++ rightString)) (leftOuts ++ rightOuts) envAfterRight)
        _ -> Left $ "Type error: + expects int+int or string+string at " ++ showPos pos
    SubtractOperator -> evalNumericBinary "-" (\leftNumber rightNumber -> leftNumber - rightNumber)
    MultiplyOperator -> evalNumericBinary "*" (\leftNumber rightNumber -> leftNumber * rightNumber)
    DivideOperator -> evalDivide
    FloorDivideOperator -> evalFloorDivide
    ModuloOperator -> evalModulo
    EqOperator -> evalEqComparison
    NotEqOperator -> evalNotEqComparison
    LtOperator -> evalNumericComparison "<" (<)
    GtOperator -> evalNumericComparison ">" (>)
    LteOperator -> evalNumericComparison "<=" (<=)
    GteOperator -> evalNumericComparison ">=" (>=)
    AndOperator -> do
      EvalExprResult leftVal leftOuts envAfterLeft <- evalExprFn env fenv leftExpr
      leftTruthy <- expectTruthy "and" pos leftVal
      if leftTruthy == 0
        then Right (EvalExprResult (IntValue 0) leftOuts envAfterLeft)
        else do
          EvalExprResult rightVal rightOuts envAfterRight <- evalExprFn envAfterLeft fenv rightExpr
          rightTruthy <- expectTruthy "and" pos rightVal
          Right (EvalExprResult (IntValue (if rightTruthy /= 0 then 1 else 0)) (leftOuts ++ rightOuts) envAfterRight)
    OrOperator -> do
      EvalExprResult leftVal leftOuts envAfterLeft <- evalExprFn env fenv leftExpr
      leftTruthy <- expectTruthy "or" pos leftVal
      if leftTruthy /= 0
        then Right (EvalExprResult (IntValue 1) leftOuts envAfterLeft)
        else do
          EvalExprResult rightVal rightOuts envAfterRight <- evalExprFn envAfterLeft fenv rightExpr
          rightTruthy <- expectTruthy "or" pos rightVal
          Right (EvalExprResult (IntValue (if rightTruthy /= 0 then 1 else 0)) (leftOuts ++ rightOuts) envAfterRight)
  where
    evalNumericBinary context opFn = do
      EvalExprResult leftVal leftOuts envAfterLeft <- evalExprFn env fenv leftExpr
      EvalExprResult rightVal rightOuts envAfterRight <- evalExprFn envAfterLeft fenv rightExpr
      leftNumber <- expectNumber context pos leftVal
      rightNumber <- expectNumber context pos rightVal
      case (leftVal, rightVal) of
        (IntValue {}, IntValue {}) -> Right (EvalExprResult (IntValue (truncate (opFn leftNumber rightNumber))) (leftOuts ++ rightOuts) envAfterRight)
        _ -> Right (EvalExprResult (FloatValue (opFn leftNumber rightNumber)) (leftOuts ++ rightOuts) envAfterRight)

    evalDivide = do
      EvalExprResult leftVal leftOuts envAfterLeft <- evalExprFn env fenv leftExpr
      EvalExprResult rightVal rightOuts envAfterRight <- evalExprFn envAfterLeft fenv rightExpr
      leftNumber <- expectNumber "/" pos leftVal
      rightNumber <- expectNumber "/" pos rightVal
      if rightNumber == 0
        then Left $ "Value error: division by zero at " ++ showPos pos
        else Right (EvalExprResult (FloatValue (leftNumber / rightNumber)) (leftOuts ++ rightOuts) envAfterRight)

    evalFloorDivide = do
      EvalExprResult leftVal leftOuts envAfterLeft <- evalExprFn env fenv leftExpr
      EvalExprResult rightVal rightOuts envAfterRight <- evalExprFn envAfterLeft fenv rightExpr
      leftNumber <- expectNumber "//" pos leftVal
      rightNumber <- expectNumber "//" pos rightVal
      if rightNumber == 0
        then Left $ "Value error: division by zero at " ++ showPos pos
        else
          let quotient = leftNumber / rightNumber
              floored = fromIntegral (floor quotient :: Int)
           in case (leftVal, rightVal) of
                (IntValue {}, IntValue {}) -> Right (EvalExprResult (IntValue (floor quotient)) (leftOuts ++ rightOuts) envAfterRight)
                _ -> Right (EvalExprResult (FloatValue floored) (leftOuts ++ rightOuts) envAfterRight)

    evalModulo = do
      EvalExprResult leftVal leftOuts envAfterLeft <- evalExprFn env fenv leftExpr
      EvalExprResult rightVal rightOuts envAfterRight <- evalExprFn envAfterLeft fenv rightExpr
      leftNumber <- expectNumber "%" pos leftVal
      rightNumber <- expectNumber "%" pos rightVal
      if rightNumber == 0
        then Left $ "Value error: modulo by zero at " ++ showPos pos
        else
          let quotient = leftNumber / rightNumber
              floored = fromIntegral (floor quotient :: Int)
              remainder = leftNumber - rightNumber * floored
           in case (leftVal, rightVal) of
                (IntValue {intValue = leftInt}, IntValue {intValue = rightInt}) -> Right (EvalExprResult (IntValue (leftInt `mod` rightInt)) (leftOuts ++ rightOuts) envAfterRight)
                _ -> Right (EvalExprResult (FloatValue remainder) (leftOuts ++ rightOuts) envAfterRight)

    evalEqComparison = do
      EvalExprResult leftVal leftOuts envAfterLeft <- evalExprFn env fenv leftExpr
      EvalExprResult rightVal rightOuts envAfterRight <- evalExprFn envAfterLeft fenv rightExpr
      case (leftVal, rightVal) of
        (IntValue {intValue = leftInt}, FloatValue {floatValue = rightFloat}) -> Right (EvalExprResult (IntValue (if (fromIntegral leftInt :: Double) == rightFloat then 1 else 0)) (leftOuts ++ rightOuts) envAfterRight)
        (FloatValue {floatValue = leftFloat}, IntValue {intValue = rightInt}) -> Right (EvalExprResult (IntValue (if leftFloat == (fromIntegral rightInt :: Double) then 1 else 0)) (leftOuts ++ rightOuts) envAfterRight)
        _ -> Right (EvalExprResult (IntValue (if leftVal == rightVal then 1 else 0)) (leftOuts ++ rightOuts) envAfterRight)

    evalNotEqComparison = do
      EvalExprResult leftVal leftOuts envAfterLeft <- evalExprFn env fenv leftExpr
      EvalExprResult rightVal rightOuts envAfterRight <- evalExprFn envAfterLeft fenv rightExpr
      case (leftVal, rightVal) of
        (IntValue {intValue = leftInt}, FloatValue {floatValue = rightFloat}) -> Right (EvalExprResult (IntValue (if (fromIntegral leftInt :: Double) /= rightFloat then 1 else 0)) (leftOuts ++ rightOuts) envAfterRight)
        (FloatValue {floatValue = leftFloat}, IntValue {intValue = rightInt}) -> Right (EvalExprResult (IntValue (if leftFloat /= (fromIntegral rightInt :: Double) then 1 else 0)) (leftOuts ++ rightOuts) envAfterRight)
        _ -> Right (EvalExprResult (IntValue (if leftVal /= rightVal then 1 else 0)) (leftOuts ++ rightOuts) envAfterRight)

    evalNumericComparison context cmp = do
      EvalExprResult leftVal leftOuts envAfterLeft <- evalExprFn env fenv leftExpr
      EvalExprResult rightVal rightOuts envAfterRight <- evalExprFn envAfterLeft fenv rightExpr
      leftNumber <- expectNumber context pos leftVal
      rightNumber <- expectNumber context pos rightVal
      Right (EvalExprResult (IntValue (if cmp leftNumber rightNumber then 1 else 0)) (leftOuts ++ rightOuts) envAfterRight)

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
    expectTruthy _ _ DictValue {dictValuePairs = pairs} = Right (if null pairs then 0 else 1)
    expectTruthy context' pos' _ = Left $ "Type error: expected int in " ++ context' ++ " at " ++ showPos pos'
