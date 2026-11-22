module PythonHS.Evaluator.EvalForStmt (evalForStmt) where

import Data.Map.Strict qualified as Map
import PythonHS.Evaluator.DictPair (DictPair (..))
import PythonHS.Evaluator.EvalExprResult (EvalExprResult (..))
import PythonHS.Evaluator.EvalForStmtConfig (EvalForStmtConfig (..))
import PythonHS.Evaluator.EvalForStmtInput (EvalForStmtInput (..))
import PythonHS.Evaluator.EvalStmtResult (EvalStmtResult (..))
import PythonHS.Evaluator.EvalStmtRet (EvalStmtRet (..))
import PythonHS.Evaluator.MaxLoopIterations (maxLoopIterations)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Parser.ExprPos (exprPos)

evalForStmt :: EvalForStmtInput -> Either String EvalStmtResult
evalForStmt input = do
  iterResult <- evalExprFn env fenv iterExpr
  let iterVal = evalExprResultValue iterResult
      iterOuts = evalExprResultOutputs iterResult
      envAfterIter = evalExprResultEnv iterResult
  case iterVal of
    IntValue {intValue = maxN} ->
      let upper = max 0 maxN
       in loopRange evalStatementsFn envAfterIter fenv 0 upper ((outputs ++) . (iterOuts ++)) 0
    ListValue {listValueItems = vals} ->
      loopList evalStatementsFn envAfterIter fenv vals ((outputs ++) . (iterOuts ++)) 0
    TupleValue {tupleValueItems = vals} ->
      loopList evalStatementsFn envAfterIter fenv vals ((outputs ++) . (iterOuts ++)) 0
    DictValue {dictValuePairs = pairs} ->
      loopList evalStatementsFn envAfterIter fenv (map dictPairKey pairs) ((outputs ++) . (iterOuts ++)) 0
    _ -> Left $ "Type error: for expects iterable (int range, list, or dict) at " ++ showPos (exprPos iterExpr)
  where
    config = evalForStmtInputConfig input
    evalStatementsFn = evalForStmtEvalStatements config
    evalExprFn = evalForStmtEvalExpr config
    env = evalForStmtInputEnv input
    fenv = evalForStmtInputFuncEnv input
    outputs = evalForStmtInputOutputs input
    name = evalForStmtInputName input
    iterExpr = evalForStmtInputIterExpr input
    body = evalForStmtInputBody input
    forPos = evalForStmtInputPos input
    rest = evalForStmtInputRest input

    loopRange runStatements env' fenv' idx upper outputAcc iterations
      | idx >= upper = runStatements env' fenv' (outputAcc []) rest
      | iterations >= maxLoopIterations = Left $ "Value error: iteration limit exceeded at " ++ showPos forPos
      | otherwise = do
          let envWithVar = Map.insert name (IntValue idx) env'
          EvalStmtResult {evalStmtResultEnv = envAfter, evalStmtResultFuncEnv = fenvAfter, evalStmtResultOutputs = outputsAfter, evalStmtResultRet = ret} <- runStatements envWithVar fenv' [] body
          let nextOutputAcc = outputAcc . (outputsAfter ++)
          let nextIterations = iterations + 1
          case ret of
            Just EvalStmtRet {evalStmtRetValue = BreakValue} -> runStatements envAfter fenvAfter (nextOutputAcc []) rest
            Just EvalStmtRet {evalStmtRetValue = ContinueValue} -> nextIterations `seq` loopRange runStatements envAfter fenvAfter (idx + 1) upper nextOutputAcc nextIterations
            Just _ ->
              Right
                EvalStmtResult
                  { evalStmtResultEnv = envAfter,
                    evalStmtResultFuncEnv = fenvAfter,
                    evalStmtResultOutputs = nextOutputAcc [],
                    evalStmtResultRet = ret
                  }
            Nothing -> nextIterations `seq` loopRange runStatements envAfter fenvAfter (idx + 1) upper nextOutputAcc nextIterations

    loopList runStatements env' fenv' [] outputAcc _ = runStatements env' fenv' (outputAcc []) rest
    loopList runStatements env' fenv' (value : remaining) outputAcc iterations
      | iterations >= maxLoopIterations = Left $ "Value error: iteration limit exceeded at " ++ showPos forPos
      | otherwise = do
          let envWithVar = Map.insert name value env'
          EvalStmtResult {evalStmtResultEnv = envAfter, evalStmtResultFuncEnv = fenvAfter, evalStmtResultOutputs = outputsAfter, evalStmtResultRet = ret} <- runStatements envWithVar fenv' [] body
          let nextOutputAcc = outputAcc . (outputsAfter ++)
          let nextIterations = iterations + 1
          case ret of
            Just EvalStmtRet {evalStmtRetValue = BreakValue} -> runStatements envAfter fenvAfter (nextOutputAcc []) rest
            Just EvalStmtRet {evalStmtRetValue = ContinueValue} -> nextIterations `seq` loopList runStatements envAfter fenvAfter remaining nextOutputAcc nextIterations
            Just _ ->
              Right
                EvalStmtResult
                  { evalStmtResultEnv = envAfter,
                    evalStmtResultFuncEnv = fenvAfter,
                    evalStmtResultOutputs = nextOutputAcc [],
                    evalStmtResultRet = ret
                  }
            Nothing -> nextIterations `seq` loopList runStatements envAfter fenvAfter remaining nextOutputAcc nextIterations
