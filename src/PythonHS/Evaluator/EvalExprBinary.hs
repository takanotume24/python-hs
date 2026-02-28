module PythonHS.Evaluator.EvalExprBinary (evalExprBinary) where

import PythonHS.AST.BinaryOperator (BinaryOperator (..))
import PythonHS.AST.Expr (Expr)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (DictValue, FloatValue, IntValue, ListValue, NoneValue, StringValue))
import PythonHS.Lexer.Position (Position)

evalExprBinary ::
  (Env -> FuncEnv -> Expr -> Either String (Value, [String], Env)) ->
  Env ->
  FuncEnv ->
  BinaryOperator ->
  Expr ->
  Expr ->
  Position ->
  Either String (Value, [String], Env)
evalExprBinary evalExprFn env fenv op leftExpr rightExpr pos =
  case op of
    AddOperator -> do
      (leftVal, leftOuts, envAfterLeft) <- evalExprFn env fenv leftExpr
      (rightVal, rightOuts, envAfterRight) <- evalExprFn envAfterLeft fenv rightExpr
      case (leftVal, rightVal) of
        (IntValue leftInt, IntValue rightInt) -> Right (IntValue (leftInt + rightInt), leftOuts ++ rightOuts, envAfterRight)
        (FloatValue leftFloat, FloatValue rightFloat) -> Right (FloatValue (leftFloat + rightFloat), leftOuts ++ rightOuts, envAfterRight)
        (IntValue leftInt, FloatValue rightFloat) -> Right (FloatValue (fromIntegral leftInt + rightFloat), leftOuts ++ rightOuts, envAfterRight)
        (FloatValue leftFloat, IntValue rightInt) -> Right (FloatValue (leftFloat + fromIntegral rightInt), leftOuts ++ rightOuts, envAfterRight)
        (StringValue leftString, StringValue rightString) -> Right (StringValue (leftString ++ rightString), leftOuts ++ rightOuts, envAfterRight)
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
      (leftVal, leftOuts, envAfterLeft) <- evalExprFn env fenv leftExpr
      leftTruthy <- expectTruthy "and" pos leftVal
      if leftTruthy == 0
        then Right (IntValue 0, leftOuts, envAfterLeft)
        else do
          (rightVal, rightOuts, envAfterRight) <- evalExprFn envAfterLeft fenv rightExpr
          rightTruthy <- expectTruthy "and" pos rightVal
          Right (IntValue (if rightTruthy /= 0 then 1 else 0), leftOuts ++ rightOuts, envAfterRight)
    OrOperator -> do
      (leftVal, leftOuts, envAfterLeft) <- evalExprFn env fenv leftExpr
      leftTruthy <- expectTruthy "or" pos leftVal
      if leftTruthy /= 0
        then Right (IntValue 1, leftOuts, envAfterLeft)
        else do
          (rightVal, rightOuts, envAfterRight) <- evalExprFn envAfterLeft fenv rightExpr
          rightTruthy <- expectTruthy "or" pos rightVal
          Right (IntValue (if rightTruthy /= 0 then 1 else 0), leftOuts ++ rightOuts, envAfterRight)
  where
    evalNumericBinary context opFn = do
      (leftVal, leftOuts, envAfterLeft) <- evalExprFn env fenv leftExpr
      (rightVal, rightOuts, envAfterRight) <- evalExprFn envAfterLeft fenv rightExpr
      leftNumber <- expectNumber context pos leftVal
      rightNumber <- expectNumber context pos rightVal
      case (leftVal, rightVal) of
        (IntValue _, IntValue _) -> Right (IntValue (truncate (opFn leftNumber rightNumber)), leftOuts ++ rightOuts, envAfterRight)
        _ -> Right (FloatValue (opFn leftNumber rightNumber), leftOuts ++ rightOuts, envAfterRight)

    evalDivide = do
      (leftVal, leftOuts, envAfterLeft) <- evalExprFn env fenv leftExpr
      (rightVal, rightOuts, envAfterRight) <- evalExprFn envAfterLeft fenv rightExpr
      leftNumber <- expectNumber "/" pos leftVal
      rightNumber <- expectNumber "/" pos rightVal
      if rightNumber == 0
        then Left $ "Value error: division by zero at " ++ showPos pos
        else Right (FloatValue (leftNumber / rightNumber), leftOuts ++ rightOuts, envAfterRight)

    evalFloorDivide = do
      (leftVal, leftOuts, envAfterLeft) <- evalExprFn env fenv leftExpr
      (rightVal, rightOuts, envAfterRight) <- evalExprFn envAfterLeft fenv rightExpr
      leftNumber <- expectNumber "//" pos leftVal
      rightNumber <- expectNumber "//" pos rightVal
      if rightNumber == 0
        then Left $ "Value error: division by zero at " ++ showPos pos
        else
          let quotient = leftNumber / rightNumber
              floored = fromIntegral (floor quotient :: Int)
           in case (leftVal, rightVal) of
                (IntValue _, IntValue _) -> Right (IntValue (floor quotient), leftOuts ++ rightOuts, envAfterRight)
                _ -> Right (FloatValue floored, leftOuts ++ rightOuts, envAfterRight)

    evalModulo = do
      (leftVal, leftOuts, envAfterLeft) <- evalExprFn env fenv leftExpr
      (rightVal, rightOuts, envAfterRight) <- evalExprFn envAfterLeft fenv rightExpr
      leftNumber <- expectNumber "%" pos leftVal
      rightNumber <- expectNumber "%" pos rightVal
      if rightNumber == 0
        then Left $ "Value error: modulo by zero at " ++ showPos pos
        else
          let quotient = leftNumber / rightNumber
              floored = fromIntegral (floor quotient :: Int)
              remainder = leftNumber - rightNumber * floored
           in case (leftVal, rightVal) of
                (IntValue leftInt, IntValue rightInt) -> Right (IntValue (leftInt `mod` rightInt), leftOuts ++ rightOuts, envAfterRight)
                _ -> Right (FloatValue remainder, leftOuts ++ rightOuts, envAfterRight)

    evalEqComparison = do
      (leftVal, leftOuts, envAfterLeft) <- evalExprFn env fenv leftExpr
      (rightVal, rightOuts, envAfterRight) <- evalExprFn envAfterLeft fenv rightExpr
      case (leftVal, rightVal) of
        (IntValue leftInt, FloatValue rightFloat) -> Right (IntValue (if (fromIntegral leftInt :: Double) == rightFloat then 1 else 0), leftOuts ++ rightOuts, envAfterRight)
        (FloatValue leftFloat, IntValue rightInt) -> Right (IntValue (if leftFloat == (fromIntegral rightInt :: Double) then 1 else 0), leftOuts ++ rightOuts, envAfterRight)
        _ -> Right (IntValue (if leftVal == rightVal then 1 else 0), leftOuts ++ rightOuts, envAfterRight)

    evalNotEqComparison = do
      (leftVal, leftOuts, envAfterLeft) <- evalExprFn env fenv leftExpr
      (rightVal, rightOuts, envAfterRight) <- evalExprFn envAfterLeft fenv rightExpr
      case (leftVal, rightVal) of
        (IntValue leftInt, FloatValue rightFloat) -> Right (IntValue (if (fromIntegral leftInt :: Double) /= rightFloat then 1 else 0), leftOuts ++ rightOuts, envAfterRight)
        (FloatValue leftFloat, IntValue rightInt) -> Right (IntValue (if leftFloat /= (fromIntegral rightInt :: Double) then 1 else 0), leftOuts ++ rightOuts, envAfterRight)
        _ -> Right (IntValue (if leftVal /= rightVal then 1 else 0), leftOuts ++ rightOuts, envAfterRight)

    evalNumericComparison context cmp = do
      (leftVal, leftOuts, envAfterLeft) <- evalExprFn env fenv leftExpr
      (rightVal, rightOuts, envAfterRight) <- evalExprFn envAfterLeft fenv rightExpr
      leftNumber <- expectNumber context pos leftVal
      rightNumber <- expectNumber context pos rightVal
      Right (IntValue (if cmp leftNumber rightNumber then 1 else 0), leftOuts ++ rightOuts, envAfterRight)

    expectNumber _ _ (IntValue n) = Right (fromIntegral n)
    expectNumber _ _ (FloatValue n) = Right n
    expectNumber _ _ NoneValue = Right 0
    expectNumber context' pos' _ = Left $ "Type error: expected int in " ++ context' ++ " at " ++ showPos pos'

    expectTruthy :: String -> Position -> Value -> Either String Int
    expectTruthy _ _ (IntValue n) = Right (if n == 0 then 0 else 1)
    expectTruthy _ _ (FloatValue n) = Right (if n == 0 then 0 else 1)
    expectTruthy _ _ NoneValue = Right 0
    expectTruthy _ _ (StringValue s) = Right (if null s then 0 else 1)
    expectTruthy _ _ (ListValue vals) = Right (if null vals then 0 else 1)
    expectTruthy _ _ (DictValue pairs) = Right (if null pairs then 0 else 1)
    expectTruthy context' pos' _ = Left $ "Type error: expected int in " ++ context' ++ " at " ++ showPos pos'
