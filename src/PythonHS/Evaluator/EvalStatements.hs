module PythonHS.Evaluator.EvalStatements (evalStatements) where

import Data.Map.Strict qualified as Map
import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalExpr (evalExpr)
import PythonHS.Evaluator.EvalExprResult (EvalExprResult (..))
import PythonHS.Evaluator.EvalForStmt (evalForStmt)
import PythonHS.Evaluator.EvalForStmtConfig (EvalForStmtConfig (..))
import PythonHS.Evaluator.EvalWhileStmt (evalWhileStmt)
import PythonHS.Evaluator.EvalWhileStmtConfig (EvalWhileStmtConfig (..))
import PythonHS.Evaluator.EvalWithStmt (evalWithStmt)
import PythonHS.Evaluator.EvalWithStmtConfig (EvalWithStmtConfig (..))
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Evaluator.ValueToOutput (valueToOutput)
import PythonHS.Lexer.Position (Position)
import PythonHS.Parser.ExprPos (exprPos)

evalStatements :: Env -> FuncEnv -> [String] -> [Stmt] -> Either String (Env, FuncEnv, [String], Maybe (Value, Position))
evalStatements env fenv outputs [] = Right (env, fenv, outputs, Nothing)
evalStatements env fenv outputs (stmt : rest) =
  case stmt of
    AssignStmt {assignStmtName, assignStmtValue} -> do
      (val, exprOuts, envAfterExpr) <- extractResult <$> evalExpr evalStatements env fenv assignStmtValue
      evalStatements (Map.insert assignStmtName val envAfterExpr) fenv (outputs ++ exprOuts) rest
    AssignUnpackStmt {assignUnpackStmtPos} ->
      Left $ "Runtime error: tuple unpack assignment is only supported in vm engine at " ++ showPos assignUnpackStmtPos
    AnnAssignStmt {annAssignStmtName, annAssignStmtValue} ->
      case annAssignStmtValue of
        Nothing -> evalStatements env fenv outputs rest
        Just expr -> do
          (val, exprOuts, envAfterExpr) <- extractResult <$> evalExpr evalStatements env fenv expr
          evalStatements (Map.insert annAssignStmtName val envAfterExpr) fenv (outputs ++ exprOuts) rest
    DecoratedStmt {decoratedStmtPos} -> Left $ "Runtime error: decorator is only supported in vm engine at " ++ showPos decoratedStmtPos
    AddAssignStmt {addAssignStmtName, addAssignStmtValue, addAssignStmtPos} -> do
      current <- lookupName env addAssignStmtName addAssignStmtPos
      (rhs, exprOuts, envAfterExpr) <- extractResult <$> evalExpr evalStatements env fenv addAssignStmtValue
      newValue <-
        case (current, rhs) of
          (IntValue {intValue = li}, IntValue {intValue = ri}) -> Right (IntValue (li + ri))
          (FloatValue {floatValue = li}, FloatValue {floatValue = ri}) -> Right (FloatValue (li + ri))
          (IntValue {intValue = li}, FloatValue {floatValue = ri}) -> Right (FloatValue (fromIntegral li + ri))
          (FloatValue {floatValue = li}, IntValue {intValue = ri}) -> Right (FloatValue (li + fromIntegral ri))
          (StringValue {stringValue = ls}, StringValue {stringValue = rs}) -> Right (StringValue (ls ++ rs))
          _ -> Left $ "Type error: + expects int+int or string+string at " ++ showPos addAssignStmtPos
      evalStatements (Map.insert addAssignStmtName newValue envAfterExpr) fenv (outputs ++ exprOuts) rest
    SubAssignStmt {subAssignStmtName, subAssignStmtValue, subAssignStmtPos} -> evalAssignNumeric subAssignStmtName subAssignStmtValue subAssignStmtPos "-=" (\li ri -> li - ri)
    MulAssignStmt {mulAssignStmtName, mulAssignStmtValue, mulAssignStmtPos} -> evalAssignNumeric mulAssignStmtName mulAssignStmtValue mulAssignStmtPos "*=" (\li ri -> li * ri)
    DivAssignStmt {divAssignStmtName, divAssignStmtValue, divAssignStmtPos} -> evalAssignDivide divAssignStmtName divAssignStmtValue divAssignStmtPos
    ModAssignStmt {modAssignStmtName, modAssignStmtValue, modAssignStmtPos} -> evalAssignModulo modAssignStmtName modAssignStmtValue modAssignStmtPos
    FloorDivAssignStmt {floorDivAssignStmtName, floorDivAssignStmtValue, floorDivAssignStmtPos} -> evalAssignFloorDivide floorDivAssignStmtName floorDivAssignStmtValue floorDivAssignStmtPos
    PrintStmt {printStmtValue} ->
      case printStmtValue of
        StringExpr {stringExprValue = s} -> evalStatements env fenv (outputs ++ [s]) rest
        _ -> do
          (val, exprOuts, envAfterExpr) <- extractResult <$> evalExpr evalStatements env fenv printStmtValue
          evalStatements envAfterExpr fenv (outputs ++ exprOuts ++ [valueToOutput val]) rest
    ReturnStmt {returnStmtValue, returnStmtPos} -> do
      (val, exprOuts, envAfterExpr) <- extractResult <$> evalExpr evalStatements env fenv returnStmtValue
      Right (envAfterExpr, fenv, outputs ++ exprOuts, Just (val, returnStmtPos))
    YieldStmt {yieldStmtPos} -> Left $ "Runtime error: yield is only supported in vm engine at " ++ showPos yieldStmtPos
    YieldFromStmt {yieldFromStmtPos} -> Left $ "Runtime error: yield from is only supported in vm engine at " ++ showPos yieldFromStmtPos
    BreakStmt {breakStmtPos} -> Right (env, fenv, outputs, Just (BreakValue, breakStmtPos))
    ContinueStmt {continueStmtPos} -> Right (env, fenv, outputs, Just (ContinueValue, continueStmtPos))
    GlobalStmt {} -> evalStatements env fenv outputs rest
    ImportStmt {} -> evalStatements env fenv outputs rest
    FromImportStmt {} -> evalStatements env fenv outputs rest
    TryExceptStmt {tryExceptStmtPos} -> Left $ "Runtime error: try/except is only supported in vm engine at " ++ showPos tryExceptStmtPos
    MatchStmt {matchStmtPos} -> Left $ "Runtime error: match/case is only supported in vm engine at " ++ showPos matchStmtPos
    RaiseStmt {raiseStmtExpr, raiseStmtPos} -> do
      (val, _, _) <- extractResult <$> evalExpr evalStatements env fenv raiseStmtExpr
      Left $ "Runtime error: " ++ valueToOutput val ++ " at " ++ showPos raiseStmtPos
    PassStmt {} -> evalStatements env fenv outputs rest
    WithStmt {withStmtContextManager, withStmtVarName, withStmtBody, withStmtPos} ->
      evalWithStmt (EvalWithStmtConfig evalStatements (evalExpr evalStatements)) env fenv outputs withStmtContextManager withStmtVarName withStmtBody withStmtPos rest
    IfStmt {ifStmtCond, ifStmtThen, ifStmtElse} -> do
      (condVal, condOuts, envAfterCond) <- extractResult <$> evalExpr evalStatements env fenv ifStmtCond
      condNum <- expectTruthy "if condition" (exprPos ifStmtCond) condVal
      if condNum /= 0
        then do
          (envThen, fenvThen, outputsThen, ret) <- evalStatements envAfterCond fenv [] ifStmtThen
          case ret of
            Just _ -> Right (envThen, fenvThen, outputs ++ condOuts ++ outputsThen, ret)
            Nothing -> evalStatements envThen fenvThen (outputs ++ condOuts ++ outputsThen) rest
        else case ifStmtElse of
          Just elseBranch -> do
            (envElse, fenvElse, outputsElse, ret) <- evalStatements envAfterCond fenv [] elseBranch
            case ret of
              Just _ -> Right (envElse, fenvElse, outputs ++ condOuts ++ outputsElse, ret)
              Nothing -> evalStatements envElse fenvElse (outputs ++ condOuts ++ outputsElse) rest
          Nothing -> evalStatements envAfterCond fenv (outputs ++ condOuts) rest
    WhileStmt {whileStmtCond, whileStmtBody, whileStmtPos} ->
      evalWhileStmt (EvalWhileStmtConfig evalStatements (evalExpr evalStatements)) env fenv outputs whileStmtCond whileStmtBody whileStmtPos rest
    ForStmt {forStmtVar, forStmtIter, forStmtBody, forStmtPos} ->
      evalForStmt (EvalForStmtConfig evalStatements (evalExpr evalStatements)) env fenv outputs forStmtVar forStmtIter forStmtBody forStmtPos rest
    ClassDefStmt {classDefStmtPos} -> Left $ "Runtime error: class is only supported in vm engine at " ++ showPos classDefStmtPos
    FunctionDefStmt {functionDefStmtName, functionDefStmtParams, functionDefStmtBody} ->
      evalStatements env (Map.insert functionDefStmtName (functionDefStmtParams, [], functionDefStmtBody) fenv) outputs rest
    FunctionDefDefaultsStmt {functionDefDefaultsStmtName, functionDefDefaultsStmtParams, functionDefDefaultsStmtDefaults, functionDefDefaultsStmtBody} ->
      evalStatements env (Map.insert functionDefDefaultsStmtName (functionDefDefaultsStmtParams, functionDefDefaultsStmtDefaults, functionDefDefaultsStmtBody) fenv) outputs rest
  where
    extractResult r = (evalExprResultValue r, evalExprResultOutputs r, evalExprResultEnv r)

    lookupName env' name pos =
      case Map.lookup name env' of
        Just value -> Right value
        Nothing -> Left $ "Name error: undefined identifier " ++ name ++ " at " ++ showPos pos

    evalAssignNumeric name expr pos context opFn = do
      current <- lookupName env name pos
      (rhs, exprOuts, envAfterExpr) <- extractResult <$> evalExpr evalStatements env fenv expr
      leftNumber <- expectNumber context pos current
      rightNumber <- expectNumber context pos rhs
      let newValue =
            case (current, rhs) of
              (IntValue _, IntValue _) -> IntValue (round (opFn leftNumber rightNumber))
              _ -> FloatValue (opFn leftNumber rightNumber)
      evalStatements (Map.insert name newValue envAfterExpr) fenv (outputs ++ exprOuts) rest

    evalAssignDivide name expr pos = do
      current <- lookupName env name pos
      (rhs, exprOuts, envAfterExpr) <- extractResult <$> evalExpr evalStatements env fenv expr
      leftNumber <- expectNumber "/=" pos current
      rightNumber <- expectNumber "/=" pos rhs
      if rightNumber == 0
        then Left $ "Value error: division by zero at " ++ showPos pos
        else do
          let newValue = FloatValue (leftNumber / rightNumber)
          evalStatements (Map.insert name newValue envAfterExpr) fenv (outputs ++ exprOuts) rest

    evalAssignFloorDivide name expr pos = do
      current <- lookupName env name pos
      (rhs, exprOuts, envAfterExpr) <- extractResult <$> evalExpr evalStatements env fenv expr
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
      (rhs, exprOuts, envAfterExpr) <- extractResult <$> evalExpr evalStatements env fenv expr
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

    expectNumber _ _ IntValue {intValue = n} = Right (fromIntegral n)
    expectNumber _ _ FloatValue {floatValue = n} = Right n
    expectNumber _ _ NoneValue = Right 0
    expectNumber context pos _ = Left $ "Type error: expected int in " ++ context ++ " at " ++ showPos pos

    expectTruthy :: String -> Position -> Value -> Either String Int
    expectTruthy _ _ IntValue {intValue = n} = Right (if n == 0 then 0 else 1)
    expectTruthy _ _ FloatValue {floatValue = n} = Right (if n == 0 then 0 else 1)
    expectTruthy _ _ NoneValue = Right 0
    expectTruthy _ _ StringValue {stringValue = s} = Right (if null s then 0 else 1)
    expectTruthy _ _ ListValue {listValueItems = vals} = Right (if null vals then 0 else 1)
    expectTruthy _ _ TupleValue {tupleValueItems = vals} = Right (if null vals then 0 else 1)
    expectTruthy _ _ DictValue {dictValuePairs = pairs} = Right (if null pairs then 0 else 1)
    expectTruthy context pos _ = Left $ "Type error: expected int in " ++ context ++ " at " ++ showPos pos
