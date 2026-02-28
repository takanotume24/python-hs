module PythonHS.Evaluator.EvalCallExpr (evalCallExpr) where

import Data.List (nub)
import qualified Data.Map.Strict as Map
import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt (..))
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalBuiltinExpr (evalBuiltinExpr)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (IntValue))
import PythonHS.Lexer.Position (Position)

evalCallExpr ::
  (Env -> FuncEnv -> [String] -> [Stmt] -> Either String (Env, FuncEnv, [String], Maybe (Value, Position))) ->
  (Env -> FuncEnv -> Expr -> Either String (Value, [String], Env)) ->
  Env ->
  FuncEnv ->
  String ->
  [Expr] ->
  Position ->
  Either String (Value, [String], Env)
evalCallExpr evalStatementsFn evalExprFn env fenv fname args pos =
  case evalBuiltinExpr evalExprFn env fenv fname args pos of
    Just result -> result
    Nothing -> case Map.lookup fname fenv of
      Nothing -> Left $ "Name error: undefined function " ++ fname ++ " at " ++ showPos pos
      Just (params, defaults, body) -> do
        (argVals, argOuts, envAfterArgs) <- evalArgs env fenv args
        if length argVals > length params
          then Left $ "Argument count mismatch when calling " ++ fname ++ " at " ++ showPos pos
          else do
            (defaultVals, defaultOuts, envAfterDefaults) <- evalMissingDefaults envAfterArgs fenv params defaults argVals
            let globalNames = collectGlobalNames body
            let allArgVals = argVals ++ defaultVals
            let localEnv = Map.union (Map.fromList (zip params allArgVals)) envAfterDefaults
            (finalEnv, _finalFenv, bodyOuts, ret) <- evalStatementsFn localEnv fenv [] body
            let retVal = case ret of
                  Just (v, _) -> v
                  Nothing -> IntValue 0
            let propagatedEnv = applyGlobalWrites envAfterDefaults finalEnv globalNames
            Right (retVal, argOuts ++ defaultOuts ++ bodyOuts, propagatedEnv)
  where
    evalArgs currentEnv currentFenv = foldl go (Right ([], [], currentEnv))
      where
        go acc expr = do
          (vals, outs, envNow) <- acc
          (value, exprOuts, envNext) <- evalExprFn envNow currentFenv expr
          Right (vals ++ [value], outs ++ exprOuts, envNext)

    collectGlobalNames stmts = nub (concatMap goStmt stmts)
      where
        goStmt (GlobalStmt name _) = [name]
        goStmt (IfStmt _ thenBranch maybeElse _) = concatMap goStmt thenBranch ++ maybe [] (concatMap goStmt) maybeElse
        goStmt (WhileStmt _ body _) = concatMap goStmt body
        goStmt (ForStmt _ _ body _) = concatMap goStmt body
        goStmt (FunctionDefStmt _ _ _ _) = []
        goStmt (FunctionDefDefaultsStmt _ _ _ _ _) = []
        goStmt _ = []

    applyGlobalWrites outerEnv finalLocalEnv names =
      foldl applyOne outerEnv names
      where
        applyOne accEnv name =
          case Map.lookup name finalLocalEnv of
            Just value -> Map.insert name value accEnv
            Nothing -> accEnv

    evalMissingDefaults currentEnv currentFenv params defaults argVals =
      fill currentEnv (drop (length argVals) params)
      where
        defaultsMap = Map.fromList defaults

        fill envNow [] = Right ([], [], envNow)
        fill envNow (missingName : remaining) =
          case Map.lookup missingName defaultsMap of
            Nothing -> Left $ "Argument count mismatch when calling " ++ fname ++ " at " ++ showPos pos
            Just defaultExpr -> do
              (v, exprOuts, envAfterExpr) <- evalExprFn envNow currentFenv defaultExpr
              (otherVals, otherOuts, finalEnv) <- fill envAfterExpr remaining
              Right (v : otherVals, exprOuts ++ otherOuts, finalEnv)
