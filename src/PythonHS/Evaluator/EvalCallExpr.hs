module PythonHS.Evaluator.EvalCallExpr (evalCallExpr) where

import Data.List (nub)
import Data.Map.Strict qualified as Map
import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.Evaluator.BindCallArgs (bindCallArgs)
import PythonHS.Evaluator.BindCallArgsInput (BindCallArgsInput (..))
import PythonHS.Evaluator.DictPair (DictPair (..))
import PythonHS.Evaluator.EvalBuiltinExpr (evalBuiltinExpr)
import PythonHS.Evaluator.EvalBuiltinExprInput (EvalBuiltinExprInput (..))
import PythonHS.Evaluator.EvalCallExprInput (EvalCallExprInput (..))
import PythonHS.Evaluator.EvalExprResult (EvalExprResult (..))
import PythonHS.Evaluator.EvalStmtResult (EvalStmtResult (..))
import PythonHS.Evaluator.EvalStmtRet (EvalStmtRet (..))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))

evalCallExpr :: EvalCallExprInput -> Either String EvalExprResult
evalCallExpr input =
  case Map.lookup fname fenv of
    Just (params, defaults, body) -> do
      (posVals, kwVals, kwPosByName, kwOrder, argOuts, envAfterArgs) <- evalCallArgs env fenv Map.empty Map.empty [] False [] args
      (bindingsWithDefaults, defaultOuts, envAfterDefaults) <-
        bindCallArgs
          BindCallArgsInput
            { bindCallArgsInputEvalExpr = evalExprFn,
              bindCallArgsInputEnv = envAfterArgs,
              bindCallArgsInputFuncEnv = fenv,
              bindCallArgsInputParams = params,
              bindCallArgsInputDefaults = defaults,
              bindCallArgsInputPosVals = posVals,
              bindCallArgsInputKwVals = kwVals,
              bindCallArgsInputKwPosByName = kwPosByName,
              bindCallArgsInputKwOrder = kwOrder,
              bindCallArgsInputFunctionName = fname,
              bindCallArgsInputPos = pos
            }
      let globalNames = collectGlobalNames body
      let localEnv = Map.union bindingsWithDefaults envAfterDefaults
      EvalStmtResult {evalStmtResultEnv = finalEnv, evalStmtResultOutputs = bodyOuts, evalStmtResultRet = ret} <- evalStatementsFn localEnv fenv [] body
      let retVal = case ret of
            Just EvalStmtRet {evalStmtRetValue = v} -> v
            Nothing -> IntValue 0
      let propagatedEnv = applyGlobalWrites envAfterDefaults finalEnv globalNames
      Right (EvalExprResult retVal (argOuts ++ defaultOuts ++ bodyOuts) propagatedEnv)
    Nothing ->
      case evalBuiltinExpr (EvalBuiltinExprInput evalExprFn env fenv fname args pos) of
        Just result ->
          case firstKeywordArg args of
            Just (_, kwPos) -> Left $ "Argument error: keyword arguments are not supported for builtin " ++ fname ++ " at " ++ showPos kwPos
            Nothing -> result
        Nothing -> Left $ "Name error: undefined function " ++ fname ++ " at " ++ showPos pos
  where
    evalStatementsFn = evalCallExprInputEvalStatementsFn input
    evalExprFn = evalCallExprInputEvalExpr input
    env = evalCallExprInputEnv input
    fenv = evalCallExprInputFuncEnv input
    fname = evalCallExprInputName input
    args = evalCallExprInputArgs input
    pos = evalCallExprInputPos input

    firstKeywordArg [] = Nothing
    firstKeywordArg (KeywordArgExpr {keywordArgExprName = argName, keywordArgExprPos = kwPos} : _) = Just (argName, kwPos)
    firstKeywordArg (_ : restArgs) = firstKeywordArg restArgs

    evalCallArgs currentEnv _ seenKw seenKwPos seenKwOrder _ positionalVals [] =
      Right (positionalVals, seenKw, seenKwPos, seenKwOrder, [], currentEnv)
    evalCallArgs currentEnv currentFenv seenKw seenKwPos seenKwOrder seenKeywordArg positionalVals (argExpr : restArgs) =
      case argExpr of
        KeywordArgExpr {keywordArgExprName = argName, keywordArgExprValue = valueExpr, keywordArgExprPos = argPos} ->
          case Map.lookup argName seenKw of
            Just _ -> Left $ "Argument error: duplicate keyword argument " ++ argName ++ " at " ++ showPos argPos
            Nothing -> do
              EvalExprResult value exprOuts envAfterExpr <- evalExprFn currentEnv currentFenv valueExpr
              (nextPosVals, nextKwVals, nextKwPosVals, nextKwOrder, restOuts, finalEnv) <-
                evalCallArgs
                  envAfterExpr
                  currentFenv
                  (Map.insert argName value seenKw)
                  (Map.insert argName argPos seenKwPos)
                  (seenKwOrder ++ [argName])
                  True
                  positionalVals
                  restArgs
              Right (nextPosVals, nextKwVals, nextKwPosVals, nextKwOrder, exprOuts ++ restOuts, finalEnv)
        StarArgExpr {starArgExprValue = valueExpr, starArgExprPos = argPos} ->
          if seenKeywordArg
            then Left $ "Argument error: positional argument follows keyword argument when calling " ++ fname ++ " at " ++ showPos argPos
            else do
              EvalExprResult value exprOuts envAfterExpr <- evalExprFn currentEnv currentFenv valueExpr
              expanded <- expandStarArg value argPos
              (nextPosVals, nextKwVals, nextKwPosVals, nextKwOrder, restOuts, finalEnv) <-
                evalCallArgs envAfterExpr currentFenv seenKw seenKwPos seenKwOrder False (positionalVals ++ expanded) restArgs
              Right (nextPosVals, nextKwVals, nextKwPosVals, nextKwOrder, exprOuts ++ restOuts, finalEnv)
        KwStarArgExpr {kwStarArgExprValue = valueExpr, kwStarArgExprPos = argPos} -> do
          EvalExprResult value exprOuts envAfterExpr <- evalExprFn currentEnv currentFenv valueExpr
          (nextKwVals, nextKwPosVals, nextKwOrder, expandOuts) <- expandKwStarArg value seenKw seenKwPos seenKwOrder argPos
          (nextPosVals, finalKwVals, finalKwPosVals, finalKwOrder, restOuts, finalEnv) <-
            evalCallArgs envAfterExpr currentFenv nextKwVals nextKwPosVals nextKwOrder True positionalVals restArgs
          Right (nextPosVals, finalKwVals, finalKwPosVals, finalKwOrder, exprOuts ++ expandOuts ++ restOuts, finalEnv)
        _ ->
          if seenKeywordArg
            then Left $ "Argument count mismatch when calling " ++ fname ++ " at " ++ showPos pos
            else do
              EvalExprResult value exprOuts envAfterExpr <- evalExprFn currentEnv currentFenv argExpr
              (nextPosVals, nextKwVals, nextKwPosVals, nextKwOrder, restOuts, finalEnv) <-
                evalCallArgs envAfterExpr currentFenv seenKw seenKwPos seenKwOrder False (positionalVals ++ [value]) restArgs
              Right (nextPosVals, nextKwVals, nextKwPosVals, nextKwOrder, exprOuts ++ restOuts, finalEnv)

    collectGlobalNames stmts = nub (concatMap goStmt stmts)
      where
        goStmt GlobalStmt {globalStmtName} = [globalStmtName]
        goStmt IfStmt {ifStmtThen, ifStmtElse} = concatMap goStmt ifStmtThen ++ maybe [] (concatMap goStmt) ifStmtElse
        goStmt WhileStmt {whileStmtBody} = concatMap goStmt whileStmtBody
        goStmt ForStmt {forStmtBody} = concatMap goStmt forStmtBody
        goStmt FunctionDefStmt {} = []
        goStmt FunctionDefDefaultsStmt {} = []
        goStmt _ = []

    applyGlobalWrites outerEnv finalLocalEnv names =
      foldl applyOne outerEnv names
      where
        applyOne accEnv name =
          case Map.lookup name finalLocalEnv of
            Just value -> Map.insert name value accEnv
            Nothing -> accEnv

    expandStarArg value argPos =
      case value of
        ListValue {listValueItems = items} -> Right items
        TupleValue {tupleValueItems = items} -> Right items
        _ -> Left $ "Type error: * argument must be list or tuple at " ++ showPos argPos

    expandKwStarArg value seenKw seenKwPos seenKwOrder argPos =
      case value of
        DictValue {dictValuePairs = pairs} -> insertPairs pairs seenKw seenKwPos seenKwOrder []
        _ -> Left $ "Type error: ** argument must be mapping at " ++ showPos argPos
      where
        insertPairs [] kwVals kwPosVals kwOrder outs = Right (kwVals, kwPosVals, kwOrder, outs)
        insertPairs (DictPair {dictPairKey = keyValue, dictPairValue = value'} : rest) kwVals kwPosVals kwOrder outs =
          case keyValue of
            StringValue {stringValue = keyName} ->
              case Map.lookup keyName kwVals of
                Just _ -> Left $ "Argument error: duplicate keyword argument " ++ keyName ++ " at " ++ showPos argPos
                Nothing ->
                  insertPairs
                    rest
                    (Map.insert keyName value' kwVals)
                    (Map.insert keyName argPos kwPosVals)
                    (kwOrder ++ [keyName])
                    outs
            _ -> Left $ "Type error: ** argument keys must be strings at " ++ showPos argPos
