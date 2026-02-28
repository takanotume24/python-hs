module PythonHS.Evaluator.EvalWhileStmt (evalWhileStmt) where

import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (BreakValue, ContinueValue, DictValue, IntValue, ListValue, NoneValue, StringValue))
import PythonHS.Lexer.Position (Position)

evalWhileStmt ::
  (Env -> FuncEnv -> [String] -> [Stmt] -> Either String (Env, FuncEnv, [String], Maybe (Value, Position))) ->
  (Env -> FuncEnv -> Expr -> Either String (Value, [String], Env)) ->
  Env ->
  FuncEnv ->
  [String] ->
  Expr ->
  [Stmt] ->
  Position ->
  [Stmt] ->
  Either String (Env, FuncEnv, [String], Maybe (Value, Position))
evalWhileStmt evalStatementsFn evalExprFn env fenv outputs cond body whilePos rest = loop env fenv id 0
  where
    loop env' fenv' outputAcc iterations = do
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
                Just (ContinueValue, _) -> nextIterations `seq` loop envAfter fenvAfter nextOutputAcc nextIterations
                Just _ -> Right (envAfter, fenvAfter, outputs ++ nextOutputAcc [], ret)
                Nothing -> nextIterations `seq` loop envAfter fenvAfter nextOutputAcc nextIterations

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

    expectTruthy _ _ (IntValue n) = Right (if n == 0 then 0 else 1)
    expectTruthy _ _ NoneValue = Right 0
    expectTruthy _ _ (StringValue s) = Right (if null s then 0 else 1)
    expectTruthy _ _ (ListValue vals) = Right (if null vals then 0 else 1)
    expectTruthy _ _ (DictValue pairs) = Right (if null pairs then 0 else 1)
    expectTruthy context pos _ = Left $ "Type error: expected int in " ++ context ++ " at " ++ showPos pos

    maxLoopIterations = 2000
