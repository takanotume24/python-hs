module PythonHS.Evaluator.EvalExpr (evalExpr) where

import Data.Bits (complement)
import Data.Map.Strict qualified as Map
import PythonHS.AST.DictEntry (DictEntry (..))
import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.ListComprehensionClause (ListComprehensionClause (..))
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Evaluator.DictPair (DictPair (..))
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalArgsGoInput (EvalArgsGoInput (..))
import PythonHS.Evaluator.EvalArgsInput (EvalArgsInput (..))
import PythonHS.Evaluator.EvalCallExpr (evalCallExpr)
import PythonHS.Evaluator.EvalComparisonChainInput (EvalComparisonChainInput (..))
import PythonHS.Evaluator.EvalComparisonItemsInput (EvalComparisonItemsInput (..))
import PythonHS.Evaluator.EvalDictEntriesInput (EvalDictEntriesInput (..))
import PythonHS.Evaluator.EvalExprBinary (evalExprBinary)
import PythonHS.Evaluator.EvalExprBinaryInput (EvalExprBinaryInput (..))
import PythonHS.Evaluator.EvalExprResult (EvalExprResult (..))
import PythonHS.Evaluator.EvalListComprehension (evalListComprehension)
import PythonHS.Evaluator.EvalListComprehensionInput (EvalListComprehensionInput (..))
import PythonHS.Evaluator.EvalStmtResult (EvalStmtResult)
import PythonHS.Evaluator.EvaluatedArgsResult (EvaluatedArgsResult (..))
import PythonHS.Evaluator.ExpectTruthy (expectTruthy)
import PythonHS.Evaluator.ExpectTruthyInput (ExpectTruthyInput (..))
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Parser.ExprPos (exprPos)
import PythonHS.VM.EvalBinaryOp (evalBinaryOp)
import PythonHS.VM.EvalBinaryOpConfig (EvalBinaryOpConfig (..))

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
      argsResult <- evalArgs EvalArgsInput {evalArgsInputEvalExpr = evalExpr evalStatementsFn, evalArgsInputEnv = env, evalArgsInputFuncEnv = fenv, evalArgsInputExprs = listExprItems}
      Right (EvalExprResult (ListValue (evaluatedArgsResultValues argsResult)) (evaluatedArgsResultOutputs argsResult) (evaluatedArgsResultEnv argsResult))
    TupleExpr {tupleExprItems} -> do
      argsResult <- evalArgs EvalArgsInput {evalArgsInputEvalExpr = evalExpr evalStatementsFn, evalArgsInputEnv = env, evalArgsInputFuncEnv = fenv, evalArgsInputExprs = tupleExprItems}
      Right (EvalExprResult (TupleValue (evaluatedArgsResultValues argsResult)) (evaluatedArgsResultOutputs argsResult) (evaluatedArgsResultEnv argsResult))
    DictExpr {dictExprEntries} -> do
      entriesResult <- evalDictEntries EvalDictEntriesInput {evalDictEntriesInputEnv = env, evalDictEntriesInputFuncEnv = fenv, evalDictEntriesInputEntries = dictExprEntries}
      Right (EvalExprResult (DictValue (evaluatedArgsResultValues entriesResult)) (evaluatedArgsResultOutputs entriesResult) (evaluatedArgsResultEnv entriesResult))
    ListComprehensionExpr {listComprehensionExprValue = valueExpr, listComprehensionExprLoopName = loopName, listComprehensionExprIter = iterExpr, listComprehensionExprPos = pos} ->
      evalListComprehension
        EvalListComprehensionInput
          { evalListComprehensionInputEvalExpr = evalExpr evalStatementsFn,
            evalListComprehensionInputEnv = env,
            evalListComprehensionInputFuncEnv = fenv,
            evalListComprehensionInputValueExpr = valueExpr,
            evalListComprehensionInputClauses = [ListComprehensionClause {listComprehensionClauseTargets = [loopName], listComprehensionClauseIter = iterExpr, listComprehensionClauseConds = []}],
            evalListComprehensionInputPos = pos
          }
    ListComprehensionClausesExpr {listComprehensionClausesExprValue = valueExpr, listComprehensionClausesExprClauses = clauses, listComprehensionClausesExprPos = pos} ->
      evalListComprehension
        EvalListComprehensionInput
          { evalListComprehensionInputEvalExpr = evalExpr evalStatementsFn,
            evalListComprehensionInputEnv = env,
            evalListComprehensionInputFuncEnv = fenv,
            evalListComprehensionInputValueExpr = valueExpr,
            evalListComprehensionInputClauses = clauses,
            evalListComprehensionInputPos = pos
          }
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
      nv <- expectTruthy ExpectTruthyInput {expectTruthyInputContext = "not", expectTruthyInputPos = exprPos expr, expectTruthyInputValue = v}
      Right (EvalExprResult (IntValue (if nv == 0 then 1 else 0)) outs envAfterExpr)
    BitwiseNotExpr {bitwiseNotExprValue, bitwiseNotExprPos} -> do
      EvalExprResult v outs envAfterExpr <- evalExpr evalStatementsFn env fenv bitwiseNotExprValue
      case v of
        IntValue {intValue = n} -> Right (EvalExprResult (IntValue (complement n)) outs envAfterExpr)
        _ -> Left $ "Type error: ~ expects int at " ++ showPos bitwiseNotExprPos
    ComparisonChainExpr {comparisonChainLeft = leftExpr, comparisonChainOps = ops, comparisonChainRights = rights, comparisonChainPos = pos} ->
      evalComparisonChain
        EvalComparisonChainInput
          { evalComparisonChainInputEvalStatementsFn = evalStatementsFn,
            evalComparisonChainInputEnv = env,
            evalComparisonChainInputFuncEnv = fenv,
            evalComparisonChainInputLeft = leftExpr,
            evalComparisonChainInputOps = ops,
            evalComparisonChainInputRights = rights,
            evalComparisonChainInputPos = pos
          }
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
    evalArgs input = go EvalArgsGoInput {evalArgsGoInputEvalExpr = evalArgsInputEvalExpr input, evalArgsGoInputFuncEnv = evalArgsInputFuncEnv input, evalArgsGoInputAcc = Right EvaluatedArgsResult {evaluatedArgsResultValues = [], evaluatedArgsResultOutputs = [], evaluatedArgsResultEnv = evalArgsInputEnv input}, evalArgsGoInputExprs = evalArgsInputExprs input}
      where
        go EvalArgsGoInput {evalArgsGoInputAcc = acc, evalArgsGoInputExprs = []} = acc
        go goInput = do
          result <- evalArgsGoInputAcc goInput
          case evalArgsGoInputExprs goInput of
            [] -> Right result
            (argExpr : restExprs) -> do
              EvalExprResult value exprOuts envNext <- evalArgsGoInputEvalExpr goInput (evaluatedArgsResultEnv result) (evalArgsGoInputFuncEnv goInput) argExpr
              go EvalArgsGoInput {evalArgsGoInputEvalExpr = evalArgsGoInputEvalExpr goInput, evalArgsGoInputFuncEnv = evalArgsGoInputFuncEnv goInput, evalArgsGoInputAcc = Right EvaluatedArgsResult {evaluatedArgsResultValues = evaluatedArgsResultValues result ++ [value], evaluatedArgsResultOutputs = evaluatedArgsResultOutputs result ++ exprOuts, evaluatedArgsResultEnv = envNext}, evalArgsGoInputExprs = restExprs}

    evalDictEntries input =
      case evalDictEntriesInputEntries input of
        [] -> Right EvaluatedArgsResult {evaluatedArgsResultValues = [], evaluatedArgsResultOutputs = [], evaluatedArgsResultEnv = evalDictEntriesInputEnv input}
        (DictEntry {dictEntryKey = keyExpr, dictEntryValue = valueExpr} : restEntries) -> do
          let currentEnv = evalDictEntriesInputEnv input
              currentFenv = evalDictEntriesInputFuncEnv input
          EvalExprResult keyVal keyOuts envAfterKey <- evalExpr evalStatementsFn currentEnv currentFenv keyExpr
          EvalExprResult valueVal valueOuts envAfterValue <- evalExpr evalStatementsFn envAfterKey currentFenv valueExpr
          restResult <- evalDictEntries input {evalDictEntriesInputEnv = envAfterValue, evalDictEntriesInputEntries = restEntries}
          Right EvaluatedArgsResult {evaluatedArgsResultValues = DictPair {dictPairKey = keyVal, dictPairValue = valueVal} : evaluatedArgsResultValues restResult, evaluatedArgsResultOutputs = keyOuts ++ valueOuts ++ evaluatedArgsResultOutputs restResult, evaluatedArgsResultEnv = evaluatedArgsResultEnv restResult}

    evalComparisonChain input = do
      let evalStatementsFn' = evalComparisonChainInputEvalStatementsFn input
          currentEnv = evalComparisonChainInputEnv input
          currentFenv = evalComparisonChainInputFuncEnv input
          leftExpr = evalComparisonChainInputLeft input
          pos' = evalComparisonChainInputPos input
      EvalExprResult leftVal leftOuts envAfterLeft <- evalExpr evalStatementsFn' currentEnv currentFenv leftExpr
      evalComparisonItems
        EvalComparisonItemsInput
          { evalComparisonItemsInputEvalStatementsFn = evalStatementsFn',
            evalComparisonItemsInputFuncEnv = currentFenv,
            evalComparisonItemsInputLeftValue = leftVal,
            evalComparisonItemsInputAccOutputs = leftOuts,
            evalComparisonItemsInputEnv = envAfterLeft,
            evalComparisonItemsInputRemainingOps = evalComparisonChainInputOps input,
            evalComparisonItemsInputRemainingRights = evalComparisonChainInputRights input,
            evalComparisonItemsInputPos = pos'
          }

    evalComparisonItems input =
      case evalComparisonItemsInputRemainingOps input of
        [] -> Right (EvalExprResult (IntValue 1) (evalComparisonItemsInputAccOutputs input) (evalComparisonItemsInputEnv input))
        (op : restOps) ->
          case evalComparisonItemsInputRemainingRights input of
            [] -> Right (EvalExprResult (IntValue 1) (evalComparisonItemsInputAccOutputs input) (evalComparisonItemsInputEnv input))
            (rightExpr : restRights) -> do
              let evalStatementsFn' = evalComparisonItemsInputEvalStatementsFn input
                  currentFenv = evalComparisonItemsInputFuncEnv input
                  leftVal = evalComparisonItemsInputLeftValue input
                  accOuts = evalComparisonItemsInputAccOutputs input
                  envAfter = evalComparisonItemsInputEnv input
                  pos' = evalComparisonItemsInputPos input
              EvalExprResult rightVal rightOuts envAfterRight <- evalExpr evalStatementsFn' envAfter currentFenv rightExpr
              case evalBinaryOp EvalBinaryOpConfig {evalBinaryOpOp = op, evalBinaryOpLeft = leftVal, evalBinaryOpRight = rightVal, evalBinaryOpPos = pos'} of
                Left err -> Left err
                Right resultVal ->
                  case resultVal of
                    IntValue {intValue = 0} -> Right (EvalExprResult (IntValue 0) (accOuts ++ rightOuts) envAfterRight)
                    _ ->
                      evalComparisonItems
                        input
                          { evalComparisonItemsInputLeftValue = rightVal,
                            evalComparisonItemsInputAccOutputs = accOuts ++ rightOuts,
                            evalComparisonItemsInputEnv = envAfterRight,
                            evalComparisonItemsInputRemainingOps = restOps,
                            evalComparisonItemsInputRemainingRights = restRights
                          }
