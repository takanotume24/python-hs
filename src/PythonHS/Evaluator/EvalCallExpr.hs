module PythonHS.Evaluator.EvalCallExpr (evalCallExpr) where

import Data.List (nub)
import qualified Data.Map.Strict as Map
import PythonHS.AST.Expr (Expr (..))
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
  case Map.lookup fname fenv of
    Just (params, defaults, body) -> do
      (posVals, kwVals, kwPosByName, kwOrder, argOuts, envAfterArgs) <- evalCallArgs env fenv Map.empty Map.empty [] False [] args
      boundByName <- bindByName params kwVals kwPosByName kwOrder
      baseBindings <- bindPositional params posVals boundByName kwPosByName kwOrder
      (bindingsWithDefaults, defaultOuts, envAfterDefaults) <- fillMissingDefaults envAfterArgs fenv params defaults baseBindings
      let globalNames = collectGlobalNames body
      let localEnv = Map.union bindingsWithDefaults envAfterDefaults
      (finalEnv, _finalFenv, bodyOuts, ret) <- evalStatementsFn localEnv fenv [] body
      let retVal = case ret of
            Just (v, _) -> v
            Nothing -> IntValue 0
      let propagatedEnv = applyGlobalWrites envAfterDefaults finalEnv globalNames
      Right (retVal, argOuts ++ defaultOuts ++ bodyOuts, propagatedEnv)
    Nothing ->
      case evalBuiltinExpr evalExprFn env fenv fname args pos of
        Just result ->
          case firstKeywordArg args of
            Just (_, kwPos) -> Left $ "Argument error: keyword arguments are not supported for builtin " ++ fname ++ " at " ++ showPos kwPos
            Nothing -> result
        Nothing -> Left $ "Name error: undefined function " ++ fname ++ " at " ++ showPos pos
  where
    firstKeywordArg [] = Nothing
    firstKeywordArg (KeywordArgExpr argName _ kwPos : _) = Just (argName, kwPos)
    firstKeywordArg (_ : restArgs) = firstKeywordArg restArgs

    evalCallArgs currentEnv _ seenKw seenKwPos seenKwOrder _ positionalVals [] =
      Right (positionalVals, seenKw, seenKwPos, seenKwOrder, [], currentEnv)
    evalCallArgs currentEnv currentFenv seenKw seenKwPos seenKwOrder seenKeywordArg positionalVals (argExpr : restArgs) =
      case argExpr of
        KeywordArgExpr argName valueExpr argPos ->
          case Map.lookup argName seenKw of
            Just _ -> Left $ "Argument error: duplicate keyword argument " ++ argName ++ " at " ++ showPos argPos
            Nothing -> do
              (value, exprOuts, envAfterExpr) <- evalExprFn currentEnv currentFenv valueExpr
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
        _ ->
          if seenKeywordArg
            then Left $ "Argument count mismatch when calling " ++ fname ++ " at " ++ showPos pos
            else do
              (value, exprOuts, envAfterExpr) <- evalExprFn currentEnv currentFenv argExpr
              (nextPosVals, nextKwVals, nextKwPosVals, nextKwOrder, restOuts, finalEnv) <-
                evalCallArgs envAfterExpr currentFenv seenKw seenKwPos seenKwOrder False (positionalVals ++ [value]) restArgs
              Right (nextPosVals, nextKwVals, nextKwPosVals, nextKwOrder, exprOuts ++ restOuts, finalEnv)

    bindByName params keywordVals kwPosByName kwOrder =
      case filter (`notElem` params) kwOrder of
        unexpectedName : _ ->
          case Map.lookup unexpectedName kwPosByName of
            Just argPos -> Left $ "Argument error: unexpected keyword argument " ++ unexpectedName ++ " at " ++ showPos argPos
            Nothing -> Left $ "Argument count mismatch when calling " ++ fname ++ " at " ++ showPos pos
        [] ->
          case filter (`notElem` params) (Map.keys keywordVals) of
            unexpectedName : _ ->
              case Map.lookup unexpectedName kwPosByName of
                Just argPos -> Left $ "Argument error: unexpected keyword argument " ++ unexpectedName ++ " at " ++ showPos argPos
                Nothing -> Left $ "Argument count mismatch when calling " ++ fname ++ " at " ++ showPos pos
            [] -> Right keywordVals

    bindPositional params positionalVals bound kwPosByName kwOrder
      | otherwise =
          case firstCollisionName of
            Just paramName ->
              case Map.lookup paramName kwPosByName of
                Just argPos -> Left $ "Argument error: multiple values for parameter " ++ paramName ++ " at " ++ showPos argPos
                Nothing -> Left $ "Argument count mismatch when calling " ++ fname ++ " at " ++ showPos pos
            Nothing ->
              if length positionalVals > length params
                then Left $ "Argument count mismatch when calling " ++ fname ++ " at " ++ showPos pos
                else Right $ foldl insertPositional bound (zip positionalParamNames positionalVals)
      where
        positionalParamNames = take (min (length positionalVals) (length params)) params
        firstCollisionName = firstMatch kwOrder positionalParamNames

        insertPositional acc (paramName, value) =
          case Map.lookup paramName acc of
            Just _ -> acc
            Nothing -> Map.insert paramName value acc

    firstMatch [] _ = Nothing
    firstMatch (name : rest) candidates
      | name `elem` candidates = Just name
      | otherwise = firstMatch rest candidates

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

    fillMissingDefaults currentEnv currentFenv params defaults boundVals =
      fill currentEnv params boundVals
      where
        defaultsMap = Map.fromList defaults

        fill envNow [] acc = Right (acc, [], envNow)
        fill envNow (paramName : remaining) acc =
          case Map.lookup paramName acc of
            Just _ -> fill envNow remaining acc
            Nothing ->
              case Map.lookup paramName defaultsMap of
                Nothing -> Left $ "Argument count mismatch when calling " ++ fname ++ " at " ++ showPos pos
                Just defaultExpr -> do
                  let defaultEnv = Map.union acc envNow
                  (v, exprOuts, envAfterExpr) <- evalExprFn defaultEnv currentFenv defaultExpr
                  (nextAcc, otherOuts, finalEnv) <- fill envAfterExpr remaining (Map.insert paramName v acc)
                  Right (nextAcc, exprOuts ++ otherOuts, finalEnv)
