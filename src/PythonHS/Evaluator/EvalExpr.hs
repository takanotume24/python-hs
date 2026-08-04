module PythonHS.Evaluator.EvalExpr (evalExpr) where

import Data.Map.Strict qualified as Map
import PythonHS.AST.DictEntry (DictEntry (..))
import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Evaluator.DictPair (DictPair (..))
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalCallExpr (evalCallExpr)
import PythonHS.Evaluator.EvalExprBinary (evalExprBinary)
import PythonHS.Evaluator.EvalExprBinaryInput (EvalExprBinaryInput (..))
import PythonHS.Evaluator.EvalExprResult (EvalExprResult (..))
import PythonHS.Evaluator.EvalStmtResult (EvalStmtResult)
import PythonHS.Evaluator.EvaluatedArgsResult (EvaluatedArgsResult (..))
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Lexer.Position (Position)

evalExpr ::
  (Env -> FuncEnv -> [String] -> [Stmt] -> Either String EvalStmtResult) ->
  Env ->
  FuncEnv ->
  Expr ->
  Either String EvalExprResult
evalExpr evalStatementsFn env fenv expr =
  case expr of
    IntegerExpr {integerExprValue} -> Right (EvalExprResult (IntValue integerExprValue) [] env)
    FloatExpr {floatExprValue} -> Right (EvalExprResult (FloatValue floatExprValue) [] env)
    StringExpr {stringExprValue} -> Right (EvalExprResult (StringValue stringExprValue) [] env)
    NoneExpr {} -> Right (EvalExprResult NoneValue [] env)
    ListExpr {listExprItems} -> do
      argsResult <- evalArgs env fenv listExprItems
      Right (EvalExprResult (ListValue (evaluatedArgsResultValues argsResult)) (evaluatedArgsResultOutputs argsResult) (evaluatedArgsResultEnv argsResult))
    TupleExpr {tupleExprItems} -> do
      argsResult <- evalArgs env fenv tupleExprItems
      Right (EvalExprResult (TupleValue (evaluatedArgsResultValues argsResult)) (evaluatedArgsResultOutputs argsResult) (evaluatedArgsResultEnv argsResult))
    DictExpr {dictExprEntries} -> do
      entriesResult <- evalDictEntries env fenv dictExprEntries
      Right (EvalExprResult (DictValue (evaluatedArgsResultValues entriesResult)) (evaluatedArgsResultOutputs entriesResult) (evaluatedArgsResultEnv entriesResult))
    ListComprehensionExpr {listComprehensionExprPos} ->
      Left $ "Runtime error: list comprehension is only supported in vm engine at " ++ showPos listComprehensionExprPos
    ListComprehensionClausesExpr {listComprehensionClausesExprPos} ->
      Left $ "Runtime error: list comprehension is only supported in vm engine at " ++ showPos listComprehensionClausesExprPos
    IdentifierExpr {identifierExprName, identifierExprPos} ->
      case Map.lookup identifierExprName env of
        Just v -> Right (EvalExprResult v [] env)
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
      EvalExprResult v outs envAfterExpr <- evalExpr evalStatementsFn env fenv unaryMinusExprValue
      case v of
        IntValue {intValue = n} -> Right (EvalExprResult (IntValue (negate n)) outs envAfterExpr)
        FloatValue {floatValue = n} -> Right (EvalExprResult (FloatValue (negate n)) outs envAfterExpr)
        _ -> Left $ "Type error: unary - expects int at " ++ showPos unaryMinusExprPos
    NotExpr {notExprValue} -> do
      EvalExprResult v outs envAfterExpr <- evalExpr evalStatementsFn env fenv notExprValue
      nv <- expectTruthy "not" (exprPos expr) v
      Right (EvalExprResult (IntValue (if nv == 0 then 1 else 0)) outs envAfterExpr)
    BinaryExpr {binaryExprOp, binaryExprLeft, binaryExprRight, binaryExprPos} ->
      evalExprBinary (EvalExprBinaryInput (evalExpr evalStatementsFn) env fenv binaryExprOp binaryExprLeft binaryExprRight binaryExprPos)
    CallExpr {callExprName, callExprArgs, callExprPos} ->
      evalCallExpr evalStatementsFn (evalExpr evalStatementsFn) env fenv callExprName callExprArgs callExprPos
    CallValueExpr {} ->
      Left $ "Runtime error: lambda is only supported in vm engine at " ++ showPos (exprPos expr)
    IndexExpr {} ->
      Left $ "Runtime error: indexing is only supported in vm engine at " ++ showPos (exprPos expr)
    SliceExpr {} ->
      Left $ "Runtime error: slicing is only supported in vm engine at " ++ showPos (exprPos expr)
  where
    evalArgs currentEnv currentFenv = foldl go (Right EvaluatedArgsResult {evaluatedArgsResultValues = [], evaluatedArgsResultOutputs = [], evaluatedArgsResultEnv = currentEnv})
      where
        go acc argExpr = do
          result <- acc
          EvalExprResult value exprOuts envNext <- evalExpr evalStatementsFn (evaluatedArgsResultEnv result) currentFenv argExpr
          Right EvaluatedArgsResult {evaluatedArgsResultValues = evaluatedArgsResultValues result ++ [value], evaluatedArgsResultOutputs = evaluatedArgsResultOutputs result ++ exprOuts, evaluatedArgsResultEnv = envNext}

    evalDictEntries currentEnv _ [] = Right EvaluatedArgsResult {evaluatedArgsResultValues = [], evaluatedArgsResultOutputs = [], evaluatedArgsResultEnv = currentEnv}
    evalDictEntries currentEnv currentFenv (DictEntry {dictEntryKey = keyExpr, dictEntryValue = valueExpr} : restEntries) = do
      EvalExprResult keyVal keyOuts envAfterKey <- evalExpr evalStatementsFn currentEnv currentFenv keyExpr
      EvalExprResult valueVal valueOuts envAfterValue <- evalExpr evalStatementsFn envAfterKey currentFenv valueExpr
      restResult <- evalDictEntries envAfterValue currentFenv restEntries
      Right EvaluatedArgsResult {evaluatedArgsResultValues = DictPair {dictPairKey = keyVal, dictPairValue = valueVal} : evaluatedArgsResultValues restResult, evaluatedArgsResultOutputs = keyOuts ++ valueOuts ++ evaluatedArgsResultOutputs restResult, evaluatedArgsResultEnv = evaluatedArgsResultEnv restResult}

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
