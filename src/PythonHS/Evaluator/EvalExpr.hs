module PythonHS.Evaluator.EvalExpr (evalExpr) where

import qualified Data.Map.Strict as Map
import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalCallExpr (evalCallExpr)
import PythonHS.Evaluator.EvalExprBinary (evalExprBinary)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Lexer.Position (Position)

evalExpr ::
  (Env -> FuncEnv -> [String] -> [Stmt] -> Either String (Env, FuncEnv, [String], Maybe (Value, Position))) ->
  Env ->
  FuncEnv ->
  Expr ->
  Either String (Value, [String], Env)
evalExpr evalStatementsFn env fenv expr =
  case expr of
    IntegerExpr {integerExprValue} -> Right (IntValue integerExprValue, [], env)
    FloatExpr {floatExprValue} -> Right (FloatValue floatExprValue, [], env)
    StringExpr {stringExprValue} -> Right (StringValue stringExprValue, [], env)
    NoneExpr {} -> Right (NoneValue, [], env)
    ListExpr {listExprItems} -> do
      (vals, outs, envAfterArgs) <- evalArgs env fenv listExprItems
      Right (ListValue vals, outs, envAfterArgs)
    TupleExpr {tupleExprItems} -> do
      (vals, outs, envAfterArgs) <- evalArgs env fenv tupleExprItems
      Right (TupleValue vals, outs, envAfterArgs)
    DictExpr {dictExprEntries} -> do
      (pairs, outs, envAfterEntries) <- evalDictEntries env fenv dictExprEntries
      Right (DictValue pairs, outs, envAfterEntries)
    ListComprehensionExpr {listComprehensionExprPos} ->
      Left $ "Runtime error: list comprehension is only supported in vm engine at " ++ showPos listComprehensionExprPos
    ListComprehensionClausesExpr {listComprehensionClausesExprPos} ->
      Left $ "Runtime error: list comprehension is only supported in vm engine at " ++ showPos listComprehensionClausesExprPos
    IdentifierExpr {identifierExprName, identifierExprPos} ->
      case Map.lookup identifierExprName env of
        Just v -> Right (v, [], env)
        Nothing -> Left $ "Name error: undefined identifier " ++ identifierExprName ++ " at " ++ showPos identifierExprPos
    KeywordArgExpr {keywordArgExprValue} ->
      evalExpr evalStatementsFn env fenv keywordArgExprValue
    StarArgExpr {starArgExprPos} ->
      Left $ "Runtime error: argument expansion is only supported in vm engine at " ++ showPos starArgExprPos
    KwStarArgExpr {kwStarArgExprPos} ->
      Left $ "Runtime error: argument expansion is only supported in vm engine at " ++ showPos kwStarArgExprPos
    WalrusExpr {} ->
      Left $ "Runtime error: walrus is only supported in vm engine at " ++ showPos (exprPos expr)
    LambdaExpr {} ->
      Left $ "Runtime error: lambda is only supported in vm engine at " ++ showPos (exprPos expr)
    LambdaDefaultsExpr {} ->
      Left $ "Runtime error: lambda is only supported in vm engine at " ++ showPos (exprPos expr)
    UnaryMinusExpr {unaryMinusExprValue, unaryMinusExprPos} -> do
      (v, outs, envAfterExpr) <- evalExpr evalStatementsFn env fenv unaryMinusExprValue
      case v of
        IntValue {intValue = n} -> Right (IntValue (negate n), outs, envAfterExpr)
        FloatValue {floatValue = n} -> Right (FloatValue (negate n), outs, envAfterExpr)
        _ -> Left $ "Type error: unary - expects int at " ++ showPos unaryMinusExprPos
    NotExpr {notExprValue} -> do
      (v, outs, envAfterExpr) <- evalExpr evalStatementsFn env fenv notExprValue
      nv <- expectTruthy "not" (exprPos expr) v
      Right (IntValue (if nv == 0 then 1 else 0), outs, envAfterExpr)
    BinaryExpr {binaryExprOp, binaryExprLeft, binaryExprRight, binaryExprPos} ->
      evalExprBinary (evalExpr evalStatementsFn) env fenv binaryExprOp binaryExprLeft binaryExprRight binaryExprPos
    CallExpr {callExprName, callExprArgs, callExprPos} ->
      evalCallExpr evalStatementsFn (evalExpr evalStatementsFn) env fenv callExprName callExprArgs callExprPos
    CallValueExpr {} ->
      Left $ "Runtime error: lambda is only supported in vm engine at " ++ showPos (exprPos expr)
    IndexExpr {} ->
      Left $ "Runtime error: indexing is only supported in vm engine at " ++ showPos (exprPos expr)
    SliceExpr {} ->
      Left $ "Runtime error: slicing is only supported in vm engine at " ++ showPos (exprPos expr)
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
    exprPos (FloatExpr _ pos) = pos
    exprPos (StringExpr _ pos) = pos
    exprPos (NoneExpr pos) = pos
    exprPos (ListExpr _ pos) = pos
    exprPos (TupleExpr _ pos) = pos
    exprPos (ListComprehensionExpr _ _ _ pos) = pos
    exprPos (ListComprehensionClausesExpr _ _ pos) = pos
    exprPos (DictExpr _ pos) = pos
    exprPos (IdentifierExpr _ pos) = pos
    exprPos (KeywordArgExpr _ _ pos) = pos
    exprPos (StarArgExpr _ pos) = pos
    exprPos (KwStarArgExpr _ pos) = pos
    exprPos (WalrusExpr _ _ pos) = pos
    exprPos (LambdaExpr _ _ pos) = pos
    exprPos (LambdaDefaultsExpr _ _ _ pos) = pos
    exprPos (UnaryMinusExpr _ pos) = pos
    exprPos (NotExpr _ pos) = pos
    exprPos (BinaryExpr _ _ _ pos) = pos
    exprPos (CallExpr _ _ pos) = pos
    exprPos (CallValueExpr _ _ pos) = pos
    exprPos (IndexExpr _ _ pos) = pos
    exprPos (SliceExpr _ _ _ pos) = pos

    expectTruthy :: String -> Position -> Value -> Either String Int
    expectTruthy _ _ IntValue {intValue = n} = Right (if n == 0 then 0 else 1)
    expectTruthy _ _ FloatValue {floatValue = n} = Right (if n == 0 then 0 else 1)
    expectTruthy _ _ NoneValue = Right 0
    expectTruthy _ _ StringValue {stringValue = s} = Right (if null s then 0 else 1)
    expectTruthy _ _ ListValue {listValueItems = vals} = Right (if null vals then 0 else 1)
    expectTruthy _ _ TupleValue {tupleValueItems = vals} = Right (if null vals then 0 else 1)
    expectTruthy _ _ DictValue {dictValuePairs = pairs} = Right (if null pairs then 0 else 1)
    expectTruthy context pos _ = Left $ "Type error: expected int in " ++ context ++ " at " ++ showPos pos
