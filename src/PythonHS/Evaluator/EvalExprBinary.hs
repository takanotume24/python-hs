module PythonHS.Evaluator.EvalExprBinary (evalExprBinary) where

import PythonHS.AST.BinaryOperator (BinaryOperator (..))
import PythonHS.AST.Expr (Expr)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (DictValue, IntValue, ListValue, NoneValue, StringValue))
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
        (StringValue leftString, StringValue rightString) -> Right (StringValue (leftString ++ rightString), leftOuts ++ rightOuts, envAfterRight)
        _ -> Left $ "Type error: + expects int+int or string+string at " ++ showPos pos
    SubtractOperator -> evalIntBinary "-" (\leftInt rightInt -> leftInt - rightInt)
    MultiplyOperator -> evalIntBinary "*" (\leftInt rightInt -> leftInt * rightInt)
    DivideOperator -> evalIntBinaryNonZero "/" "Value error: division by zero at " (\leftInt rightInt -> leftInt `div` rightInt)
    FloorDivideOperator -> evalIntBinaryNonZero "//" "Value error: division by zero at " (\leftInt rightInt -> leftInt `div` rightInt)
    ModuloOperator -> evalIntBinaryNonZero "%" "Value error: modulo by zero at " (\leftInt rightInt -> leftInt `mod` rightInt)
    EqOperator -> evalComparison (==)
    NotEqOperator -> evalComparison (/=)
    LtOperator -> evalIntComparison "<" (<)
    GtOperator -> evalIntComparison ">" (>)
    LteOperator -> evalIntComparison "<=" (<=)
    GteOperator -> evalIntComparison ">=" (>=)
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
    evalIntBinary context opFn = do
      (leftVal, leftOuts, envAfterLeft) <- evalExprFn env fenv leftExpr
      (rightVal, rightOuts, envAfterRight) <- evalExprFn envAfterLeft fenv rightExpr
      leftInt <- expectInt context pos leftVal
      rightInt <- expectInt context pos rightVal
      Right (IntValue (opFn leftInt rightInt), leftOuts ++ rightOuts, envAfterRight)

    evalIntBinaryNonZero context errPrefix opFn = do
      (leftVal, leftOuts, envAfterLeft) <- evalExprFn env fenv leftExpr
      (rightVal, rightOuts, envAfterRight) <- evalExprFn envAfterLeft fenv rightExpr
      leftInt <- expectInt context pos leftVal
      rightInt <- expectInt context pos rightVal
      if rightInt == 0
        then Left $ errPrefix ++ showPos pos
        else Right (IntValue (opFn leftInt rightInt), leftOuts ++ rightOuts, envAfterRight)

    evalComparison cmp = do
      (leftVal, leftOuts, envAfterLeft) <- evalExprFn env fenv leftExpr
      (rightVal, rightOuts, envAfterRight) <- evalExprFn envAfterLeft fenv rightExpr
      Right (IntValue (if cmp leftVal rightVal then 1 else 0), leftOuts ++ rightOuts, envAfterRight)

    evalIntComparison context cmp = do
      (leftVal, leftOuts, envAfterLeft) <- evalExprFn env fenv leftExpr
      (rightVal, rightOuts, envAfterRight) <- evalExprFn envAfterLeft fenv rightExpr
      leftInt <- expectInt context pos leftVal
      rightInt <- expectInt context pos rightVal
      Right (IntValue (if cmp leftInt rightInt then 1 else 0), leftOuts ++ rightOuts, envAfterRight)

    expectInt _ _ (IntValue n) = Right n
    expectInt _ _ NoneValue = Right 0
    expectInt context' pos' _ = Left $ "Type error: expected int in " ++ context' ++ " at " ++ showPos pos'

    expectTruthy _ _ (IntValue n) = Right (if n == 0 then 0 else 1)
    expectTruthy _ _ NoneValue = Right 0
    expectTruthy _ _ (StringValue s) = Right (if null s then 0 else 1)
    expectTruthy _ _ (ListValue vals) = Right (if null vals then 0 else 1)
    expectTruthy _ _ (DictValue pairs) = Right (if null pairs then 0 else 1)
    expectTruthy context' pos' _ = Left $ "Type error: expected int in " ++ context' ++ " at " ++ showPos pos'
