module PythonHS.Evaluator.EvalStatements (evalStatements) where

import qualified Data.Map.Strict as Map
import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalExpr (evalExpr)
import PythonHS.Evaluator.EvalForStmt (evalForStmt)
import PythonHS.Evaluator.EvalWhileStmt (evalWhileStmt)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (BreakValue, ContinueValue, DictValue, FloatValue, IntValue, ListValue, NoneValue, StringValue))
import PythonHS.Evaluator.ValueToOutput (valueToOutput)
import PythonHS.Lexer.Position (Position)

evalStatements :: Env -> FuncEnv -> [String] -> [Stmt] -> Either String (Env, FuncEnv, [String], Maybe (Value, Position))
evalStatements env fenv outputs [] = Right (env, fenv, outputs, Nothing)
evalStatements env fenv outputs (stmt : rest) =
  case stmt of
    AssignStmt name expr _ -> do
      (val, exprOuts, envAfterExpr) <- evalExpr evalStatements env fenv expr
      evalStatements (Map.insert name val envAfterExpr) fenv (outputs ++ exprOuts) rest

    AddAssignStmt name expr pos -> do
      current <- lookupName env name pos
      (rhs, exprOuts, envAfterExpr) <- evalExpr evalStatements env fenv expr
      newValue <-
        case (current, rhs) of
          (IntValue li, IntValue ri) -> Right (IntValue (li + ri))
          (FloatValue li, FloatValue ri) -> Right (FloatValue (li + ri))
          (IntValue li, FloatValue ri) -> Right (FloatValue (fromIntegral li + ri))
          (FloatValue li, IntValue ri) -> Right (FloatValue (li + fromIntegral ri))
          (StringValue ls, StringValue rs) -> Right (StringValue (ls ++ rs))
          _ -> Left $ "Type error: + expects int+int or string+string at " ++ showPos pos
      evalStatements (Map.insert name newValue envAfterExpr) fenv (outputs ++ exprOuts) rest

    SubAssignStmt name expr pos -> evalAssignNumeric name expr pos "-=" (\li ri -> li - ri)
    MulAssignStmt name expr pos -> evalAssignNumeric name expr pos "*=" (\li ri -> li * ri)
    DivAssignStmt name expr pos -> evalAssignDivide name expr pos
    ModAssignStmt name expr pos -> evalAssignModulo name expr pos
    FloorDivAssignStmt name expr pos -> evalAssignFloorDivide name expr pos

    PrintStmt expr _ ->
      case expr of
        StringExpr s _ -> evalStatements env fenv (outputs ++ [s]) rest
        _ -> do
          (val, exprOuts, envAfterExpr) <- evalExpr evalStatements env fenv expr
          evalStatements envAfterExpr fenv (outputs ++ exprOuts ++ [valueToOutput val]) rest

    ReturnStmt expr pos -> do
      (val, exprOuts, envAfterExpr) <- evalExpr evalStatements env fenv expr
      Right (envAfterExpr, fenv, outputs ++ exprOuts, Just (val, pos))

    BreakStmt pos -> Right (env, fenv, outputs, Just (BreakValue, pos))
    ContinueStmt pos -> Right (env, fenv, outputs, Just (ContinueValue, pos))
    GlobalStmt _ _ -> evalStatements env fenv outputs rest
    ImportStmt _ _ -> evalStatements env fenv outputs rest
    PassStmt _ -> evalStatements env fenv outputs rest

    IfStmt cond thenBranch maybeElse _ -> do
      (condVal, condOuts, envAfterCond) <- evalExpr evalStatements env fenv cond
      condNum <- expectTruthy "if condition" (exprPos cond) condVal
      if condNum /= 0
        then do
          (envThen, fenvThen, outputsThen, ret) <- evalStatements envAfterCond fenv [] thenBranch
          case ret of
            Just _ -> Right (envThen, fenvThen, outputs ++ condOuts ++ outputsThen, ret)
            Nothing -> evalStatements envThen fenvThen (outputs ++ condOuts ++ outputsThen) rest
        else case maybeElse of
          Just elseBranch -> do
            (envElse, fenvElse, outputsElse, ret) <- evalStatements envAfterCond fenv [] elseBranch
            case ret of
              Just _ -> Right (envElse, fenvElse, outputs ++ condOuts ++ outputsElse, ret)
              Nothing -> evalStatements envElse fenvElse (outputs ++ condOuts ++ outputsElse) rest
          Nothing -> evalStatements envAfterCond fenv (outputs ++ condOuts) rest

    WhileStmt cond body whilePos ->
      evalWhileStmt evalStatements (evalExpr evalStatements) env fenv outputs cond body whilePos rest

    ForStmt name iterExpr body forPos ->
      evalForStmt evalStatements (evalExpr evalStatements) env fenv outputs name iterExpr body forPos rest

    FunctionDefStmt name params body _ ->
      evalStatements env (Map.insert name (params, [], body) fenv) outputs rest

    FunctionDefDefaultsStmt name params defaults body _ ->
      evalStatements env (Map.insert name (params, defaults, body) fenv) outputs rest
  where
    lookupName env' name pos =
      case Map.lookup name env' of
        Just value -> Right value
        Nothing -> Left $ "Name error: undefined identifier " ++ name ++ " at " ++ showPos pos

    evalAssignNumeric name expr pos context opFn = do
      current <- lookupName env name pos
      (rhs, exprOuts, envAfterExpr) <- evalExpr evalStatements env fenv expr
      leftNumber <- expectNumber context pos current
      rightNumber <- expectNumber context pos rhs
      let newValue =
            case (current, rhs) of
              (IntValue _, IntValue _) -> IntValue (round (opFn leftNumber rightNumber))
              _ -> FloatValue (opFn leftNumber rightNumber)
      evalStatements (Map.insert name newValue envAfterExpr) fenv (outputs ++ exprOuts) rest

    evalAssignDivide name expr pos = do
      current <- lookupName env name pos
      (rhs, exprOuts, envAfterExpr) <- evalExpr evalStatements env fenv expr
      leftNumber <- expectNumber "/=" pos current
      rightNumber <- expectNumber "/=" pos rhs
      if rightNumber == 0
        then Left $ "Value error: division by zero at " ++ showPos pos
        else do
          let newValue = FloatValue (leftNumber / rightNumber)
          evalStatements (Map.insert name newValue envAfterExpr) fenv (outputs ++ exprOuts) rest

    evalAssignFloorDivide name expr pos = do
      current <- lookupName env name pos
      (rhs, exprOuts, envAfterExpr) <- evalExpr evalStatements env fenv expr
      leftNumber <- expectNumber "//=" pos current
      rightNumber <- expectNumber "//=" pos rhs
      if rightNumber == 0
        then Left $ "Value error: division by zero at " ++ showPos pos
        else do
          let quotient = leftNumber / rightNumber
              newValue =
                case (current, rhs) of
                  (IntValue _, IntValue _) -> IntValue (floor quotient)
                  _ -> FloatValue (fromIntegral (floor quotient :: Int))
          evalStatements (Map.insert name newValue envAfterExpr) fenv (outputs ++ exprOuts) rest

    evalAssignModulo name expr pos = do
      current <- lookupName env name pos
      (rhs, exprOuts, envAfterExpr) <- evalExpr evalStatements env fenv expr
      leftNumber <- expectNumber "%=" pos current
      rightNumber <- expectNumber "%=" pos rhs
      if rightNumber == 0
        then Left $ "Value error: modulo by zero at " ++ showPos pos
        else do
          let quotient = leftNumber / rightNumber
              floored = fromIntegral (floor quotient :: Int)
              remainder = leftNumber - rightNumber * floored
              newValue =
                case (current, rhs) of
                  (IntValue li, IntValue ri) -> IntValue (li `mod` ri)
                  _ -> FloatValue remainder
          evalStatements (Map.insert name newValue envAfterExpr) fenv (outputs ++ exprOuts) rest

    expectNumber _ _ (IntValue n) = Right (fromIntegral n)
    expectNumber _ _ (FloatValue n) = Right n
    expectNumber _ _ NoneValue = Right 0
    expectNumber context pos _ = Left $ "Type error: expected int in " ++ context ++ " at " ++ showPos pos

    expectTruthy :: String -> Position -> Value -> Either String Int
    expectTruthy _ _ (IntValue n) = Right (if n == 0 then 0 else 1)
    expectTruthy _ _ (FloatValue n) = Right (if n == 0 then 0 else 1)
    expectTruthy _ _ NoneValue = Right 0
    expectTruthy _ _ (StringValue s) = Right (if null s then 0 else 1)
    expectTruthy _ _ (ListValue vals) = Right (if null vals then 0 else 1)
    expectTruthy _ _ (DictValue pairs) = Right (if null pairs then 0 else 1)
    expectTruthy context pos _ = Left $ "Type error: expected int in " ++ context ++ " at " ++ showPos pos

    exprPos (IntegerExpr _ pos) = pos
    exprPos (FloatExpr _ pos) = pos
    exprPos (StringExpr _ pos) = pos
    exprPos (NoneExpr pos) = pos
    exprPos (ListExpr _ pos) = pos
    exprPos (DictExpr _ pos) = pos
    exprPos (IdentifierExpr _ pos) = pos
    exprPos (KeywordArgExpr _ _ pos) = pos
    exprPos (UnaryMinusExpr _ pos) = pos
    exprPos (NotExpr _ pos) = pos
    exprPos (BinaryExpr _ _ _ pos) = pos
    exprPos (CallExpr _ _ pos) = pos
