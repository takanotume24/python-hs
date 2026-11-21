module PythonHS.Evaluator.BindCallArgs (bindCallArgs) where

import Data.List (find)
import Data.Map.Strict qualified as Map
import PythonHS.AST.FunctionDefault (FunctionDefault (..))
import PythonHS.Evaluator.BindCallArgsInput (BindCallArgsInput (..))
import PythonHS.Evaluator.DictPair (DictPair (..))
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalExprResult (EvalExprResult (..))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))

bindCallArgs :: BindCallArgsInput -> Either String (Map.Map String Value, [String], Env)
bindCallArgs input = do
  let evalExprFn = bindCallArgsInputEvalExpr input
      currentEnv = bindCallArgsInputEnv input
      currentFenv = bindCallArgsInputFuncEnv input
      params = bindCallArgsInputParams input
      defaults = bindCallArgsInputDefaults input
      posVals = bindCallArgsInputPosVals input
      kwVals = bindCallArgsInputKwVals input
      kwPosByName = bindCallArgsInputKwPosByName input
      kwOrder = bindCallArgsInputKwOrder input
      fname = bindCallArgsInputFunctionName input
      pos = bindCallArgsInputPos input
      normalParams = filter isNormalParam params
      varArgName = fmap stripStars (find isVarArgParam params)
      varKwName = fmap stripStars (find isVarKwParam params)
  boundByName <- bindByName normalParams varKwName kwVals kwPosByName kwOrder fname pos
  baseBindings <- bindPositional normalParams varArgName posVals boundByName kwPosByName kwOrder fname pos
  let bindingsWithVarKw = case varKwName of Nothing -> baseBindings; Just name -> Map.insert name (buildKwDict kwOrder normalParams kwVals) baseBindings
  fillMissingDefaults currentEnv currentFenv normalParams defaults bindingsWithVarKw fname pos evalExprFn
  where
    bindByName normalParams varKwName keywordVals kwPosByName kwOrder fname pos =
      case varKwName of
        Just _ -> Right keywordVals
        Nothing ->
          case filter (`notElem` normalParams) kwOrder of
            unexpectedName : _ ->
              case Map.lookup unexpectedName kwPosByName of
                Just argPos -> Left $ "Argument error: unexpected keyword argument " ++ unexpectedName ++ " at " ++ showPos argPos
                Nothing -> Left $ "Argument count mismatch when calling " ++ fname ++ " at " ++ showPos pos
            [] ->
              case filter (`notElem` normalParams) (Map.keys keywordVals) of
                unexpectedName : _ ->
                  case Map.lookup unexpectedName kwPosByName of
                    Just argPos -> Left $ "Argument error: unexpected keyword argument " ++ unexpectedName ++ " at " ++ showPos argPos
                    Nothing -> Left $ "Argument count mismatch when calling " ++ fname ++ " at " ++ showPos pos
                [] -> Right keywordVals

    bindPositional normalParams varArgName positionalVals bound kwPosByName kwOrder fname pos =
      case firstCollisionName of
        Just paramName ->
          case Map.lookup paramName kwPosByName of
            Just argPos -> Left $ "Argument error: multiple values for parameter " ++ paramName ++ " at " ++ showPos argPos
            Nothing -> Left $ "Argument count mismatch when calling " ++ fname ++ " at " ++ showPos pos
        Nothing ->
          if length positionalVals > normalCount && varArgName == Nothing
            then Left $ "Argument count mismatch when calling " ++ fname ++ " at " ++ showPos pos
            else Right $ foldl insertPositional initialBound (zip positionalParamNames positionalVals)
      where
        normalCount = length normalParams
        positionalParamNames = take (min (length positionalVals) normalCount) normalParams
        firstCollisionName = firstMatch kwOrder positionalParamNames
        initialBound = case varArgName of Nothing -> bound; Just name -> Map.insert name (ListValue (drop normalCount positionalVals)) bound

        insertPositional acc (paramName, value) =
          case Map.lookup paramName acc of
            Just _ -> acc
            Nothing -> Map.insert paramName value acc

    firstMatch [] _ = Nothing
    firstMatch (name : rest) candidates
      | name `elem` candidates = Just name
      | otherwise = firstMatch rest candidates

    fillMissingDefaults currentEnv currentFenv normalParams defaults boundVals fname pos evalExprFn =
      fill currentEnv normalParams boundVals
      where
        defaultsMap = Map.fromList (map (\FunctionDefault {functionDefaultParam = p, functionDefaultExpr = e} -> (p, e)) defaults)

        fill envNow [] acc = Right (acc, [], envNow)
        fill envNow (paramName : remaining) acc =
          case Map.lookup paramName acc of
            Just _ -> fill envNow remaining acc
            Nothing ->
              case Map.lookup paramName defaultsMap of
                Nothing -> Left $ "Argument count mismatch when calling " ++ fname ++ " at " ++ showPos pos
                Just defaultExpr -> do
                  let defaultEnv = Map.union acc envNow
                  EvalExprResult v exprOuts envAfterExpr <- evalExprFn defaultEnv currentFenv defaultExpr
                  (nextAcc, otherOuts, finalEnv) <- fill envAfterExpr remaining (Map.insert paramName v acc)
                  Right (nextAcc, exprOuts ++ otherOuts, finalEnv)

    isVarArgParam ('*' : '*' : _) = False
    isVarArgParam ('*' : _) = True
    isVarArgParam _ = False

    isVarKwParam ('*' : '*' : _) = True
    isVarKwParam _ = False

    isNormalParam p = not (isVarArgParam p || isVarKwParam p)

    stripStars p
      | isVarKwParam p = drop 2 p
      | isVarArgParam p = drop 1 p
      | otherwise = p

    buildKwDict kwOrder normalParams keywordVals =
      DictValue
        [ DictPair {dictPairKey = StringValue name, dictPairValue = value}
          | name <- kwOrder,
            name `notElem` normalParams,
            Just value <- [Map.lookup name keywordVals]
        ]
