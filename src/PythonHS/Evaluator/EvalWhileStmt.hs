module PythonHS.Evaluator.EvalWhileStmt (evalWhileStmt) where

import PythonHS.Evaluator.EvalExprResult (EvalExprResult (..))
import PythonHS.Evaluator.EvalStmtResult (EvalStmtResult (..))
import PythonHS.Evaluator.EvalStmtRet (EvalStmtRet (..))
import PythonHS.Evaluator.EvalWhileStmtConfig (EvalWhileStmtConfig (..))
import PythonHS.Evaluator.EvalWhileStmtInput (EvalWhileStmtInput (..))
import PythonHS.Evaluator.MaxLoopIterations (maxLoopIterations)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Lexer.Position (Position)
import PythonHS.Parser.ExprPos (exprPos)

evalWhileStmt :: EvalWhileStmtInput -> Either String EvalStmtResult
evalWhileStmt input = loop env fenv id 0
  where
    config = evalWhileStmtInputConfig input
    env = evalWhileStmtInputEnv input
    fenv = evalWhileStmtInputFuncEnv input
    outputs = evalWhileStmtInputOutputs input
    cond = evalWhileStmtInputCond input
    body = evalWhileStmtInputBody input
    whilePos = evalWhileStmtInputPos input
    rest = evalWhileStmtInputRest input
    evalStatementsFn = evalWhileStmtEvalStatements config
    evalExprFn = evalWhileStmtEvalExpr config

    loop env' fenv' outputAcc iterations = do
      condResult <- evalExprFn env' fenv' cond
      let condVal = evalExprResultValue condResult
          condOuts = evalExprResultOutputs condResult
          envAfterCond = evalExprResultEnv condResult
      condNum <- expectTruthy "while condition" (exprPos cond) condVal
      if condNum == 0
        then evalStatementsFn envAfterCond fenv' (outputs ++ outputAcc condOuts) rest
        else
          if iterations >= maxLoopIterations
            then Left $ "Value error: iteration limit exceeded at " ++ showPos whilePos
            else do
              EvalStmtResult {evalStmtResultEnv = envAfter, evalStmtResultFuncEnv = fenvAfter, evalStmtResultOutputs = outputsAfter, evalStmtResultRet = ret} <- evalStatementsFn envAfterCond fenv' [] body
              let nextOutputAcc = outputAcc . (condOuts ++) . (outputsAfter ++)
              let nextIterations = iterations + 1
              case ret of
                Just EvalStmtRet {evalStmtRetValue = BreakValue} -> evalStatementsFn envAfter fenvAfter (outputs ++ nextOutputAcc []) rest
                Just EvalStmtRet {evalStmtRetValue = ContinueValue} -> nextIterations `seq` loop envAfter fenvAfter nextOutputAcc nextIterations
                Just _ ->
                  Right
                    EvalStmtResult
                      { evalStmtResultEnv = envAfter,
                        evalStmtResultFuncEnv = fenvAfter,
                        evalStmtResultOutputs = outputs ++ nextOutputAcc [],
                        evalStmtResultRet = ret
                      }
                Nothing -> nextIterations `seq` loop envAfter fenvAfter nextOutputAcc nextIterations

    expectTruthy :: String -> Position -> Value -> Either String Int
    expectTruthy _ _ IntValue {intValue = n} = Right (if n == 0 then 0 else 1)
    expectTruthy _ _ FloatValue {floatValue = n} = Right (if n == 0 then 0 else 1)
    expectTruthy _ _ NoneValue = Right 0
    expectTruthy _ _ StringValue {stringValue = s} = Right (if null s then 0 else 1)
    expectTruthy _ _ ListValue {listValueItems = vals} = Right (if null vals then 0 else 1)
    expectTruthy _ _ TupleValue {tupleValueItems = vals} = Right (if null vals then 0 else 1)
    expectTruthy _ _ DictValue {dictValuePairs = pairs} = Right (if null pairs then 0 else 1)
    expectTruthy context pos' _ = Left $ "Type error: expected int in " ++ context ++ " at " ++ showPos pos'
