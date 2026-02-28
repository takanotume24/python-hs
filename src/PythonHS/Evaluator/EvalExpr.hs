module PythonHS.Evaluator.EvalExpr (evalExpr) where

import qualified Data.Map.Strict as Map
import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalCallExpr (evalCallExpr)
import PythonHS.Evaluator.EvalExprBinary (evalExprBinary)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (DictValue, IntValue, ListValue, NoneValue, StringValue))
import PythonHS.Lexer.Position (Position)

evalExpr ::
  (Env -> FuncEnv -> [String] -> [Stmt] -> Either String (Env, FuncEnv, [String], Maybe (Value, Position))) ->
  Env ->
  FuncEnv ->
  Expr ->
  Either String (Value, [String], Env)
evalExpr evalStatementsFn env fenv expr =
  case expr of
    IntegerExpr n _ -> Right (IntValue n, [], env)
    StringExpr s _ -> Right (StringValue s, [], env)
    NoneExpr _ -> Right (NoneValue, [], env)
    ListExpr exprs _ -> do
      (vals, outs, envAfterArgs) <- evalArgs env fenv exprs
      Right (ListValue vals, outs, envAfterArgs)
    DictExpr entries _ -> do
      (pairs, outs, envAfterEntries) <- evalDictEntries env fenv entries
      Right (DictValue pairs, outs, envAfterEntries)
    IdentifierExpr name pos ->
      case Map.lookup name env of
        Just v -> Right (v, [], env)
        Nothing -> Left $ "Name error: undefined identifier " ++ name ++ " at " ++ showPos pos
    KeywordArgExpr _ valueExpr _ ->
      evalExpr evalStatementsFn env fenv valueExpr
    UnaryMinusExpr unaryExpr pos -> do
      (v, outs, envAfterExpr) <- evalExpr evalStatementsFn env fenv unaryExpr
      case v of
        IntValue n -> Right (IntValue (negate n), outs, envAfterExpr)
        _ -> Left $ "Type error: unary - expects int at " ++ showPos pos
    NotExpr notExpr _ -> do
      (v, outs, envAfterExpr) <- evalExpr evalStatementsFn env fenv notExpr
      nv <- expectTruthy "not" (exprPos notExpr) v
      Right (IntValue (if nv == 0 then 1 else 0), outs, envAfterExpr)
    BinaryExpr op leftExpr rightExpr pos ->
      evalExprBinary (evalExpr evalStatementsFn) env fenv op leftExpr rightExpr pos
    CallExpr fname args pos ->
      evalCallExpr evalStatementsFn (evalExpr evalStatementsFn) env fenv fname args pos
  where
    evalArgs currentEnv currentFenv = foldl go (Right ([], [], currentEnv))
      where
        go acc argExpr = do
          (vals, outs, envNow) <- acc
          (value, exprOuts, envNext) <- evalExpr evalStatementsFn envNow currentFenv argExpr
          Right (vals ++ [value], outs ++ exprOuts, envNext)

    evalDictEntries currentEnv _ [] = Right ([], [], currentEnv)
    evalDictEntries currentEnv currentFenv ((keyExpr, valueExpr) : restEntries) = do
      (keyVal, keyOuts, envAfterKey) <- evalExpr evalStatementsFn currentEnv currentFenv keyExpr
      (valueVal, valueOuts, envAfterValue) <- evalExpr evalStatementsFn envAfterKey currentFenv valueExpr
      (restVals, restOuts, envAfterRest) <- evalDictEntries envAfterValue currentFenv restEntries
      Right ((keyVal, valueVal) : restVals, keyOuts ++ valueOuts ++ restOuts, envAfterRest)

    exprPos (IntegerExpr _ pos) = pos
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

    expectTruthy :: String -> Position -> Value -> Either String Int
    expectTruthy _ _ (IntValue n) = Right (if n == 0 then 0 else 1)
    expectTruthy _ _ NoneValue = Right 0
    expectTruthy _ _ (StringValue s) = Right (if null s then 0 else 1)
    expectTruthy _ _ (ListValue vals) = Right (if null vals then 0 else 1)
    expectTruthy _ _ (DictValue pairs) = Right (if null pairs then 0 else 1)
    expectTruthy context pos _ = Left $ "Type error: expected int in " ++ context ++ " at " ++ showPos pos
