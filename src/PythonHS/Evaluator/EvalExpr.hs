module PythonHS.Evaluator.EvalExpr (evalExpr) where

import Data.Bits (complement)
import Data.Map.Strict qualified as Map
import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.ListComprehensionClause (ListComprehensionClause (..))
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalArgs (evalArgs)
import PythonHS.Evaluator.EvalArgsInput (EvalArgsInput (..))
import PythonHS.Evaluator.EvalCallExpr (evalCallExpr)
import PythonHS.Evaluator.EvalCallExprInput (EvalCallExprInput (..))
import PythonHS.Evaluator.EvalComparisonChain (evalComparisonChain)
import PythonHS.Evaluator.EvalComparisonChainInput (EvalComparisonChainInput (..))
import PythonHS.Evaluator.EvalComparisonItems (evalComparisonItems)
import PythonHS.Evaluator.EvalComparisonItemsInput (EvalComparisonItemsInput (..))
import PythonHS.Evaluator.EvalDictEntries (evalDictEntries)
import PythonHS.Evaluator.EvalDictEntriesInput (EvalDictEntriesInput (..))
import PythonHS.Evaluator.EvalExprBinary (evalExprBinary)
import PythonHS.Evaluator.EvalExprBinaryInput (EvalExprBinaryInput (..))
import PythonHS.Evaluator.EvalExprInput (EvalExprInput (..))
import PythonHS.Evaluator.EvalExprResult (EvalExprResult (..))
import PythonHS.Evaluator.EvalIndexExpr (evalIndexExpr)
import PythonHS.Evaluator.EvalIndexExprInput (EvalIndexExprInput (..))
import PythonHS.Evaluator.EvalListComprehension (evalListComprehension)
import PythonHS.Evaluator.EvalListComprehensionInput (EvalListComprehensionInput (..))
import PythonHS.Evaluator.EvalSliceExpr (evalSliceExpr)
import PythonHS.Evaluator.EvalSliceExprInput (EvalSliceExprInput (..))
import PythonHS.Evaluator.EvaluatedArgsResult (EvaluatedArgsResult (..))
import PythonHS.Evaluator.ExpectTruthy (expectTruthy)
import PythonHS.Evaluator.ExpectTruthyInput (ExpectTruthyInput (..))
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Parser.ExprPos (exprPos)

evalExpr :: EvalExprInput -> Either String EvalExprResult
evalExpr input =
  let evalStatementsFn = evalExprInputEvalStatementsFn input
      env = evalExprInputEnv input
      fenv = evalExprInputFuncEnv input
      evalSub :: Env -> FuncEnv -> Expr -> Either String EvalExprResult
      evalSub env' fenv' expr' = evalExpr input {evalExprInputEnv = env', evalExprInputFuncEnv = fenv', evalExprInputExpr = expr'}
   in case evalExprInputExpr input of
        IntegerExpr {integerExprValue} -> Right (EvalExprResult {evalExprResultValue = IntValue integerExprValue, evalExprResultOutputs = [], evalExprResultEnv = env})
        FloatExpr {floatExprValue} -> Right (EvalExprResult {evalExprResultValue = FloatValue floatExprValue, evalExprResultOutputs = [], evalExprResultEnv = env})
        StringExpr {stringExprValue} -> Right (EvalExprResult {evalExprResultValue = StringValue stringExprValue, evalExprResultOutputs = [], evalExprResultEnv = env})
        NoneExpr {} -> Right (EvalExprResult {evalExprResultValue = NoneValue, evalExprResultOutputs = [], evalExprResultEnv = env})
        ListExpr {listExprItems} -> do
          argsResult <- evalArgs EvalArgsInput {evalArgsInputEvalExpr = evalSub, evalArgsInputEnv = env, evalArgsInputFuncEnv = fenv, evalArgsInputExprs = listExprItems}
          Right (EvalExprResult {evalExprResultValue = ListValue (evaluatedArgsResultValues argsResult), evalExprResultOutputs = evaluatedArgsResultOutputs argsResult, evalExprResultEnv = evaluatedArgsResultEnv argsResult})
        TupleExpr {tupleExprItems} -> do
          argsResult <- evalArgs EvalArgsInput {evalArgsInputEvalExpr = evalSub, evalArgsInputEnv = env, evalArgsInputFuncEnv = fenv, evalArgsInputExprs = tupleExprItems}
          Right (EvalExprResult {evalExprResultValue = TupleValue (evaluatedArgsResultValues argsResult), evalExprResultOutputs = evaluatedArgsResultOutputs argsResult, evalExprResultEnv = evaluatedArgsResultEnv argsResult})
        DictExpr {dictExprEntries} -> do
          entriesResult <- evalDictEntries EvalDictEntriesInput {evalDictEntriesInputEvalExpr = evalSub, evalDictEntriesInputEnv = env, evalDictEntriesInputFuncEnv = fenv, evalDictEntriesInputEntries = dictExprEntries}
          Right (EvalExprResult {evalExprResultValue = DictValue (evaluatedArgsResultValues entriesResult), evalExprResultOutputs = evaluatedArgsResultOutputs entriesResult, evalExprResultEnv = evaluatedArgsResultEnv entriesResult})
        ListComprehensionExpr {listComprehensionExprValue = valueExpr, listComprehensionExprLoopName = loopName, listComprehensionExprIter = iterExpr, listComprehensionExprPos = pos} ->
          evalListComprehension
            EvalListComprehensionInput
              { evalListComprehensionInputEvalExpr = evalSub,
                evalListComprehensionInputEnv = env,
                evalListComprehensionInputFuncEnv = fenv,
                evalListComprehensionInputValueExpr = valueExpr,
                evalListComprehensionInputClauses = [ListComprehensionClause {listComprehensionClauseTargets = [loopName], listComprehensionClauseIter = iterExpr, listComprehensionClauseConds = []}],
                evalListComprehensionInputPos = pos
              }
        ListComprehensionClausesExpr {listComprehensionClausesExprValue = valueExpr, listComprehensionClausesExprClauses = clauses, listComprehensionClausesExprPos = pos} ->
          evalListComprehension
            EvalListComprehensionInput
              { evalListComprehensionInputEvalExpr = evalSub,
                evalListComprehensionInputEnv = env,
                evalListComprehensionInputFuncEnv = fenv,
                evalListComprehensionInputValueExpr = valueExpr,
                evalListComprehensionInputClauses = clauses,
                evalListComprehensionInputPos = pos
              }
        IdentifierExpr {identifierExprName, identifierExprPos} ->
          case Map.lookup identifierExprName env of
            Just v -> Right (EvalExprResult {evalExprResultValue = v, evalExprResultOutputs = [], evalExprResultEnv = env})
            Nothing -> Left $ "Name error: undefined identifier " ++ identifierExprName ++ " at " ++ showPos identifierExprPos
        KeywordArgExpr {keywordArgExprValue} ->
          evalSub env fenv keywordArgExprValue
        StarArgExpr {starArgExprPos} ->
          Left $ "Runtime error: argument expansion is only supported in vm engine at " ++ showPos starArgExprPos
        KwStarArgExpr {kwStarArgExprPos} ->
          Left $ "Runtime error: argument expansion is only supported in vm engine at " ++ showPos kwStarArgExprPos
        WalrusExpr {walrusExprName = name, walrusExprValue = valueExpr} -> do
          EvalExprResult {evalExprResultValue = value, evalExprResultOutputs = outs, evalExprResultEnv = envAfterValue} <- evalSub env fenv valueExpr
          Right (EvalExprResult {evalExprResultValue = value, evalExprResultOutputs = outs, evalExprResultEnv = Map.insert name value envAfterValue})
        LambdaExpr {} ->
          Left $ "Runtime error: lambda is only supported in vm engine at " ++ showPos (exprPos (evalExprInputExpr input))
        LambdaDefaultsExpr {} ->
          Left $ "Runtime error: lambda is only supported in vm engine at " ++ showPos (exprPos (evalExprInputExpr input))
        UnaryMinusExpr {unaryMinusExprValue, unaryMinusExprPos} -> do
          EvalExprResult {evalExprResultValue = v, evalExprResultOutputs = outs, evalExprResultEnv = envAfterExpr} <- evalSub env fenv unaryMinusExprValue
          case v of
            IntValue {intValue = n} -> Right (EvalExprResult {evalExprResultValue = IntValue (negate n), evalExprResultOutputs = outs, evalExprResultEnv = envAfterExpr})
            FloatValue {floatValue = n} -> Right (EvalExprResult {evalExprResultValue = FloatValue (negate n), evalExprResultOutputs = outs, evalExprResultEnv = envAfterExpr})
            _ -> Left $ "Type error: unary - expects int at " ++ showPos unaryMinusExprPos
        NotExpr {notExprValue} -> do
          EvalExprResult {evalExprResultValue = v, evalExprResultOutputs = outs, evalExprResultEnv = envAfterExpr} <- evalSub env fenv notExprValue
          nv <- expectTruthy ExpectTruthyInput {expectTruthyInputContext = "not", expectTruthyInputPos = exprPos (evalExprInputExpr input), expectTruthyInputValue = v}
          Right (EvalExprResult {evalExprResultValue = IntValue (if nv == 0 then 1 else 0), evalExprResultOutputs = outs, evalExprResultEnv = envAfterExpr})
        BitwiseNotExpr {bitwiseNotExprValue, bitwiseNotExprPos} -> do
          EvalExprResult {evalExprResultValue = v, evalExprResultOutputs = outs, evalExprResultEnv = envAfterExpr} <- evalSub env fenv bitwiseNotExprValue
          case v of
            IntValue {intValue = n} -> Right (EvalExprResult {evalExprResultValue = IntValue (complement n), evalExprResultOutputs = outs, evalExprResultEnv = envAfterExpr})
            _ -> Left $ "Type error: ~ expects int at " ++ showPos bitwiseNotExprPos
        ComparisonChainExpr {comparisonChainLeft = leftExpr, comparisonChainOps = ops, comparisonChainRights = rights, comparisonChainPos = pos} ->
          evalComparisonChain
            EvalComparisonChainInput
              { evalComparisonChainInputEvalStatementsFn = evalStatementsFn,
                evalComparisonChainInputEvalExpr = evalSub,
                evalComparisonChainInputEnv = env,
                evalComparisonChainInputFuncEnv = fenv,
                evalComparisonChainInputLeft = leftExpr,
                evalComparisonChainInputOps = ops,
                evalComparisonChainInputRights = rights,
                evalComparisonChainInputPos = pos
              }
        BinaryExpr {binaryExprOp, binaryExprLeft, binaryExprRight, binaryExprPos} ->
          evalExprBinary (EvalExprBinaryInput {evalExprBinaryEvalExprFn = evalSub, evalExprBinaryEnv = env, evalExprBinaryFuncEnv = fenv, evalExprBinaryOp = binaryExprOp, evalExprBinaryLeftExpr = binaryExprLeft, evalExprBinaryRightExpr = binaryExprRight, evalExprBinaryPos = binaryExprPos})
        CallExpr {callExprName, callExprArgs, callExprPos} ->
          evalCallExpr
            EvalCallExprInput
              { evalCallExprInputEvalStatementsFn = evalStatementsFn,
                evalCallExprInputEvalExpr = evalSub,
                evalCallExprInputEnv = env,
                evalCallExprInputFuncEnv = fenv,
                evalCallExprInputName = callExprName,
                evalCallExprInputArgs = callExprArgs,
                evalCallExprInputPos = callExprPos
              }
        CallValueExpr {} ->
          Left $ "Runtime error: lambda is only supported in vm engine at " ++ showPos (exprPos (evalExprInputExpr input))
        IndexExpr {indexExprBase = baseExpr, indexExprIndex = idxExpr, indexExprPos = pos} ->
          evalIndexExpr
            EvalIndexExprInput
              { evalIndexExprInputEvalExpr = evalSub,
                evalIndexExprInputEnv = env,
                evalIndexExprInputFuncEnv = fenv,
                evalIndexExprInputBase = baseExpr,
                evalIndexExprInputIndex = idxExpr,
                evalIndexExprInputPos = pos
              }
        SliceExpr {sliceExprBase = baseExpr, sliceExprStart = maybeStartExpr, sliceExprEnd = maybeEndExpr, sliceExprPos = pos} ->
          evalSliceExpr
            EvalSliceExprInput
              { evalSliceExprInputEvalExpr = evalSub,
                evalSliceExprInputEnv = env,
                evalSliceExprInputFuncEnv = fenv,
                evalSliceExprInputBase = baseExpr,
                evalSliceExprInputStart = maybeStartExpr,
                evalSliceExprInputEnd = maybeEndExpr,
                evalSliceExprInputPos = pos
              }
