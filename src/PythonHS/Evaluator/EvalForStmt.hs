module PythonHS.Evaluator.EvalForStmt (evalForStmt) where

import qualified Data.Map.Strict as Map
import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Evaluator.MaxLoopIterations (maxLoopIterations)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Lexer.Position (Position)
import PythonHS.Parser.ExprPos (exprPos)

evalForStmt ::
  (Env -> FuncEnv -> [String] -> [Stmt] -> Either String (Env, FuncEnv, [String], Maybe (Value, Position))) ->
  (Env -> FuncEnv -> Expr -> Either String (Value, [String], Env)) ->
  Env ->
  FuncEnv ->
  [String] ->
  String ->
  Expr ->
  [Stmt] ->
  Position ->
  [Stmt] ->
  Either String (Env, FuncEnv, [String], Maybe (Value, Position))
evalForStmt evalStatementsFn evalExprFn env fenv outputs name iterExpr body forPos rest = do
  (iterVal, iterOuts, envAfterIter) <- evalExprFn env fenv iterExpr
  case iterVal of
    IntValue {intValue = maxN} ->
      let upper = max 0 maxN
       in loopRange envAfterIter fenv 0 upper ((outputs ++) . (iterOuts ++)) 0
    ListValue {listValueItems = vals} ->
      loopList envAfterIter fenv vals ((outputs ++) . (iterOuts ++)) 0
    TupleValue {tupleValueItems = vals} ->
      loopList envAfterIter fenv vals ((outputs ++) . (iterOuts ++)) 0
    DictValue {dictValuePairs = pairs} ->
      loopList envAfterIter fenv (map fst pairs) ((outputs ++) . (iterOuts ++)) 0
    _ -> Left $ "Type error: for expects iterable (int range, list, or dict) at " ++ showPos (exprPos iterExpr)
  where
    loopRange env' fenv' idx upper outputAcc iterations
      | idx >= upper = evalStatementsFn env' fenv' (outputAcc []) rest
      | iterations >= maxLoopIterations = Left $ "Value error: iteration limit exceeded at " ++ showPos forPos
      | otherwise = do
          let envWithVar = Map.insert name (IntValue idx) env'
          (envAfter, fenvAfter, outputsAfter, ret) <- evalStatementsFn envWithVar fenv' [] body
          let nextOutputAcc = outputAcc . (outputsAfter ++)
          let nextIterations = iterations + 1
          case ret of
            Just (BreakValue, _) -> evalStatementsFn envAfter fenvAfter (nextOutputAcc []) rest
            Just (ContinueValue, _) -> nextIterations `seq` loopRange envAfter fenvAfter (idx + 1) upper nextOutputAcc nextIterations
            Just _ -> Right (envAfter, fenvAfter, nextOutputAcc [], ret)
            Nothing -> nextIterations `seq` loopRange envAfter fenvAfter (idx + 1) upper nextOutputAcc nextIterations

    loopList env' fenv' [] outputAcc _ = evalStatementsFn env' fenv' (outputAcc []) rest
    loopList env' fenv' (value : remaining) outputAcc iterations
      | iterations >= maxLoopIterations = Left $ "Value error: iteration limit exceeded at " ++ showPos forPos
      | otherwise = do
          let envWithVar = Map.insert name value env'
          (envAfter, fenvAfter, outputsAfter, ret) <- evalStatementsFn envWithVar fenv' [] body
          let nextOutputAcc = outputAcc . (outputsAfter ++)
          let nextIterations = iterations + 1
          case ret of
            Just (BreakValue, _) -> evalStatementsFn envAfter fenvAfter (nextOutputAcc []) rest
            Just (ContinueValue, _) -> nextIterations `seq` loopList envAfter fenvAfter remaining nextOutputAcc nextIterations
            Just _ -> Right (envAfter, fenvAfter, nextOutputAcc [], ret)
            Nothing -> nextIterations `seq` loopList envAfter fenvAfter remaining nextOutputAcc nextIterations
