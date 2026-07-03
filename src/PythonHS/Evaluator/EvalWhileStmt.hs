module PythonHS.Evaluator.EvalWhileStmt (evalWhileStmt) where

import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalWhileStmtConfig (EvalWhileStmtConfig (..))
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Evaluator.MaxLoopIterations (maxLoopIterations)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Lexer.Position (Position)
import PythonHS.Parser.ExprPos (exprPos)

evalWhileStmt ::
  EvalWhileStmtConfig ->
  Env ->
  FuncEnv ->
  [String] ->
  Expr ->
  [Stmt] ->
  Position ->
  [Stmt] ->
  Either String (Env, FuncEnv, [String], Maybe (Value, Position))
evalWhileStmt config env fenv outputs cond body whilePos rest =
  let evalStatementsFn = evalWhileStmtEvalStatements config
      evalExprFn = evalWhileStmtEvalExpr config
   in loop evalStatementsFn evalExprFn env fenv id 0
  where
    loop evalStatementsFn evalExprFn env' fenv' outputAcc iterations = do
      (condVal, condOuts, envAfterCond) <- evalExprFn env' fenv' cond
      condNum <- expectTruthy "while condition" (exprPos cond) condVal
      if condNum == 0
        then evalStatementsFn envAfterCond fenv' (outputs ++ outputAcc condOuts) rest
        else
          if iterations >= maxLoopIterations
            then Left $ "Value error: iteration limit exceeded at " ++ showPos whilePos
            else do
              (envAfter, fenvAfter, outputsAfter, ret) <- evalStatementsFn envAfterCond fenv' [] body
              let nextOutputAcc = outputAcc . (condOuts ++) . (outputsAfter ++)
              let nextIterations = iterations + 1
              case ret of
                Just (BreakValue, _) -> evalStatementsFn envAfter fenvAfter (outputs ++ nextOutputAcc []) rest
                Just (ContinueValue, _) -> nextIterations `seq` loop evalStatementsFn evalExprFn envAfter fenvAfter nextOutputAcc nextIterations
                Just _ -> Right (envAfter, fenvAfter, outputs ++ nextOutputAcc [], ret)
                Nothing -> nextIterations `seq` loop evalStatementsFn evalExprFn envAfter fenvAfter nextOutputAcc nextIterations

    expectTruthy :: String -> Position -> Value -> Either String Int
    expectTruthy _ _ IntValue {intValue = n} = Right (if n == 0 then 0 else 1)
    expectTruthy _ _ FloatValue {floatValue = n} = Right (if n == 0 then 0 else 1)
    expectTruthy _ _ NoneValue = Right 0
    expectTruthy _ _ StringValue {stringValue = s} = Right (if null s then 0 else 1)
    expectTruthy _ _ ListValue {listValueItems = vals} = Right (if null vals then 0 else 1)
    expectTruthy _ _ TupleValue {tupleValueItems = vals} = Right (if null vals then 0 else 1)
    expectTruthy _ _ DictValue {dictValuePairs = pairs} = Right (if null pairs then 0 else 1)
    expectTruthy context pos _ = Left $ "Type error: expected int in " ++ context ++ " at " ++ showPos pos
