module PythonHS.Evaluator.EvalListComprehension (evalListComprehension) where

import Data.Map.Strict qualified as Map
import PythonHS.AST.Expr (Expr)
import PythonHS.AST.ListComprehensionClause (ListComprehensionClause (..))
import PythonHS.Evaluator.DictPair (DictPair (..))
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalExprResult (EvalExprResult (..))
import PythonHS.Evaluator.EvalListComprehensionInput (EvalListComprehensionInput (..))
import PythonHS.Evaluator.ExpectTruthy (expectTruthy)
import PythonHS.Evaluator.ExpectTruthyInput (ExpectTruthyInput (..))
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.ToForIterable (toForIterable)
import PythonHS.VM.ToForIterableConfig (ToForIterableConfig (..))

evalListComprehension :: EvalListComprehensionInput -> Either String EvalExprResult
evalListComprehension input = do
  let outerEnv = evalListComprehensionInputEnv input
  (items, finalEnv, outputs) <- evalClauses (evalListComprehensionInputClauses input) outerEnv [] []
  let resultEnv = Map.intersection finalEnv outerEnv
  Right (EvalExprResult (ListValue items) outputs resultEnv)
  where
    evalExprFn = evalListComprehensionInputEvalExpr input
    fenv = evalListComprehensionInputFuncEnv input
    valueExpr = evalListComprehensionInputValueExpr input
    pos = evalListComprehensionInputPos input

    evalClauses :: [ListComprehensionClause Expr] -> Env -> [Value] -> [String] -> Either String ([Value], Env, [String])
    evalClauses [] env acc outputs = do
      EvalExprResult value valueOuts envAfterValue <- evalExprFn env fenv valueExpr
      Right (acc ++ [value], envAfterValue, outputs ++ valueOuts)
    evalClauses (ListComprehensionClause {listComprehensionClauseTargets = targets, listComprehensionClauseIter = iterExpr, listComprehensionClauseConds = conds} : rest) env acc outputs = do
      EvalExprResult iterValue iterOuts envAfterIter <- evalExprFn env fenv iterExpr
      iterItems <- toForIterable ToForIterableConfig {toForIterableValue = iterValue, toForIterablePos = pos}
      evalClauseItems targets conds rest envAfterIter iterItems acc (outputs ++ iterOuts)

    evalClauseItems :: [String] -> [Expr] -> [ListComprehensionClause Expr] -> Env -> [Value] -> [Value] -> [String] -> Either String ([Value], Env, [String])
    evalClauseItems _ _ _ env [] acc outputs = Right (acc, env, outputs)
    evalClauseItems targets conds restClauses env (item : restItems) acc outputs = do
      nextEnv <- bindTargets targets item env
      (shouldInclude, envAfterConds, condOuts) <- evaluateConditions conds nextEnv []
      (accAfter, envAfterRest, outputsAfterRest) <-
        if shouldInclude
          then evalClauses restClauses envAfterConds acc (outputs ++ condOuts)
          else Right (acc, envAfterConds, outputs ++ condOuts)
      evalClauseItems targets conds restClauses envAfterRest restItems accAfter outputsAfterRest

    evaluateConditions :: [Expr] -> Env -> [String] -> Either String (Bool, Env, [String])
    evaluateConditions [] env condOuts = Right (True, env, condOuts)
    evaluateConditions (condExpr : restConds) env condOuts = do
      EvalExprResult condValue condOuts' envAfterCond <- evalExprFn env fenv condExpr
      truthy <- expectTruthy ExpectTruthyInput {expectTruthyInputContext = "list comprehension condition", expectTruthyInputPos = pos, expectTruthyInputValue = condValue}
      let totalOuts = condOuts ++ condOuts'
      if truthy == 1
        then evaluateConditions restConds envAfterCond totalOuts
        else Right (False, envAfterCond, totalOuts)

    bindTargets :: [String] -> Value -> Env -> Either String Env
    bindTargets [] _ _ = Left ("Value error: empty comprehension target at " ++ showPos pos)
    bindTargets [name] value env = Right (Map.insert name value env)
    bindTargets names value env =
      case value of
        ListValue {listValueItems = vals} ->
          if length vals == length names
            then Right (foldl bindOne env (zip names vals))
            else Left ("Value error: unpacking mismatch in comprehension at " ++ showPos pos)
        _ -> Left ("Type error: unpacking expects list value in comprehension at " ++ showPos pos)

    bindOne :: Env -> (String, Value) -> Env
    bindOne envNow (name, value) = Map.insert name value envNow
