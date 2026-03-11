module PythonHS.VM.ExecuteCallFunction (executeCallFunction) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (ClassValue, FunctionRefValue, InstanceValue, IntValue, ModuleValue), Value)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.BindCallArguments (bindCallArguments)
import PythonHS.VM.BindDefaults (bindDefaults)
import PythonHS.VM.CallBuiltin (callBuiltin)
import PythonHS.VM.CollectFunctionGlobalDecls (collectFunctionGlobalDecls)
import PythonHS.VM.EvaluateBuiltinArgs (evaluateBuiltinArgs)
import PythonHS.VM.ExecuteCallValueFunction (executeCallValueFunction)
import PythonHS.VM.EvaluateUserArgs (evaluateUserArgs)
import PythonHS.VM.FindMethodFunctionName (findMethodFunctionName)
import PythonHS.VM.FirstKeywordArg (firstKeywordArg)
import PythonHS.VM.Instruction (Instruction)
import PythonHS.VM.LookupName (lookupName)
import PythonHS.VM.ModulePrefixFor (modulePrefixFor)

executeCallFunction ::
  ([Instruction] -> Int -> [Value] -> Map.Map String Value -> Map.Map String Value -> Map.Map String ([String], [(String, [Instruction])], [Instruction]) -> Set.Set String -> Map.Map Int [Value] -> Map.Map Int Int -> [Int] -> [String] -> Bool -> Either String (Maybe Value, Map.Map String Value, Map.Map String ([String], [(String, [Instruction])], [Instruction]), [String])) ->
  Bool ->
  String ->
  [([Instruction], Maybe String, Position)] ->
  Position ->
  [Value] ->
  Map.Map String Value ->
  Map.Map String Value ->
  Map.Map String ([String], [(String, [Instruction])], [Instruction]) ->
  [String] ->
  Either String ([Value], Map.Map String Value, Map.Map String Value, Map.Map String ([String], [(String, [Instruction])], [Instruction]), [String])
executeCallFunction execute isTopLevel fname compiledArgs pos stack globalsEnv localEnv functions outputs =
  case lookupName fname localEnv globalsEnv of
    Just callableValue@(FunctionRefValue _ _) ->
      executeCallValueFunction execute isTopLevel compiledArgs pos (callableValue : stack) globalsEnv localEnv functions outputs
    _ ->
      case Map.lookup fname functions of
        Just (params, defaultCodes, functionCode) -> do
          (argValues, argKinds, globalsAfterArgs, functionsAfterArgs, outputsAfterArgs) <-
            evaluateUserArgs execute fname pos localEnv compiledArgs globalsEnv functions outputs False Set.empty [] []
          initialLocals <- bindCallArguments fname pos params argValues argKinds
          (functionLocals, globalsAfterDefaults, functionsAfterDefaults, outputsAfterDefaults) <-
            bindDefaults execute fname pos params defaultCodes initialLocals globalsAfterArgs functionsAfterArgs outputsAfterArgs
          let functionGlobalDecls = collectFunctionGlobalDecls functionCode
          (maybeValue, newGlobals, newFunctions, newOutputs) <-
            execute functionCode 0 [] globalsAfterDefaults functionLocals functionsAfterDefaults functionGlobalDecls Map.empty Map.empty [] outputsAfterDefaults False
          let returnValue =
                case maybeValue of
                  Just value -> value
                  Nothing -> IntValue 0
          let newLocalEnv = if isTopLevel then newGlobals else localEnv
          Right (returnValue : stack, newGlobals, newLocalEnv, newFunctions, newOutputs)
        Nothing ->
          case firstKeywordArg compiledArgs of
            Just (_, argPos)
              | isBuiltinName fname ->
                  Left ("Argument error: keyword arguments are not supported for builtin " ++ fname ++ " at " ++ showPos argPos)
            _ -> do
              (args, globalsAfterArgs, functionsAfterArgs, outputsAfterArgs) <-
                evaluateBuiltinArgs execute localEnv compiledArgs globalsEnv functions outputs []
              case lookupName fname localEnv globalsEnv of
                Just (ClassValue className _ _) ->
                  createInstance className args globalsAfterArgs functionsAfterArgs outputsAfterArgs
                _ ->
                  case args of
                    ClassValue className _ _ : methodArgs@(InstanceValue _ _ : _) ->
                      case findMethodFunctionName globalsAfterArgs localEnv className fname of
                        Just methodFunctionName ->
                          callUserFunction methodFunctionName Nothing methodArgs globalsAfterArgs functionsAfterArgs outputsAfterArgs
                        Nothing -> callBuiltinOrFail args globalsAfterArgs functionsAfterArgs outputsAfterArgs
                    InstanceValue className _ : _ ->
                      case findMethodFunctionName globalsAfterArgs localEnv className fname of
                        Just methodFunctionName ->
                          callUserFunction methodFunctionName Nothing args globalsAfterArgs functionsAfterArgs outputsAfterArgs
                        Nothing -> callBuiltinOrFail args globalsAfterArgs functionsAfterArgs outputsAfterArgs
                    _ -> callBuiltinOrFail args globalsAfterArgs functionsAfterArgs outputsAfterArgs
  where
    callUserFunction targetName maybeCapturedBindings args globalsNow functionsNow outputsNow =
      case Map.lookup targetName functionsNow of
        Nothing -> Left ("Name error: undefined function " ++ targetName ++ " at " ++ showPos pos)
        Just (params, defaultCodes, functionCode) -> do
          let argKinds = map (\_ -> (Nothing, pos)) args
          initialLocals <- bindCallArguments targetName pos params args argKinds
          let capturedLocals =
                case maybeCapturedBindings of
                  Just bindings -> Map.fromList bindings
                  Nothing -> Map.empty
              mergedLocals = Map.union initialLocals capturedLocals
          (functionLocals, globalsAfterDefaults, functionsAfterDefaults, outputsAfterDefaults) <-
            bindDefaults execute targetName pos params defaultCodes mergedLocals globalsNow functionsNow outputsNow
          let functionGlobalDecls = collectFunctionGlobalDecls functionCode
          (maybeValue, newGlobals, newFunctions, newOutputs) <-
            execute functionCode 0 [] globalsAfterDefaults functionLocals functionsAfterDefaults functionGlobalDecls Map.empty Map.empty [] outputsAfterDefaults False
          let returnValue =
                case maybeValue of
                  Just value -> value
                  Nothing -> IntValue 0
          let newLocalEnv = if isTopLevel then newGlobals else localEnv
          Right (returnValue : stack, newGlobals, newLocalEnv, newFunctions, newOutputs)

    createInstance className args globalsNow functionsNow outputsNow =
      let instanceValue = InstanceValue className []
       in case findMethodFunctionName globalsNow localEnv className "__init__" of
            Just initFunctionName ->
              case Map.lookup initFunctionName functionsNow of
                Nothing -> Left ("Name error: undefined function " ++ initFunctionName ++ " at " ++ showPos pos)
                Just (initParams, initDefaults, initCode) -> do
                  let initArgValues = instanceValue : args
                      initArgKinds = map (\_ -> (Nothing, pos)) initArgValues
                  initialLocals <- bindCallArguments initFunctionName pos initParams initArgValues initArgKinds
                  (functionLocals, globalsAfterDefaults, functionsAfterDefaults, outputsAfterDefaults) <-
                    bindDefaults execute initFunctionName pos initParams initDefaults initialLocals globalsNow functionsNow outputsNow
                  let functionGlobalDecls = collectFunctionGlobalDecls initCode
                  (maybeValue, newGlobals, newFunctions, newOutputs) <-
                    execute initCode 0 [] globalsAfterDefaults functionLocals functionsAfterDefaults functionGlobalDecls Map.empty Map.empty [] outputsAfterDefaults False
                  let constructedInstance =
                        case maybeValue of
                          Just instanceResult@(InstanceValue _ _) -> instanceResult
                          _ -> instanceValue
                  let newLocalEnv = if isTopLevel then newGlobals else localEnv
                  Right (constructedInstance : stack, newGlobals, newLocalEnv, newFunctions, newOutputs)
            Nothing ->
              let newLocalEnv = if isTopLevel then globalsNow else localEnv
               in Right (instanceValue : stack, globalsNow, newLocalEnv, functionsNow, outputsNow)

    callBuiltinOrFail args globalsAfterArgs functionsAfterArgs outputsAfterArgs =
      case callModuleMemberFunction args globalsAfterArgs functionsAfterArgs outputsAfterArgs of
        Just result -> result
        Nothing ->
          case firstKeywordArg compiledArgs of
            Just (_, argPos)
              | isBuiltinName fname ->
                  Left ("Argument error: keyword arguments are not supported for builtin " ++ fname ++ " at " ++ showPos argPos)
            Nothing ->
              case callBuiltin fname args pos of
                Just (Left err) -> Left err
                Just (Right builtinValue) ->
                  let newLocalEnv = if isTopLevel then globalsAfterArgs else localEnv
                   in Right (builtinValue : stack, globalsAfterArgs, newLocalEnv, functionsAfterArgs, outputsAfterArgs)
                Nothing -> Left ("Name error: undefined function " ++ fname ++ " at " ++ showPos pos)
            _ ->
              case callBuiltin fname args pos of
                Just (Left err) -> Left err
                Just (Right builtinValue) ->
                  let newLocalEnv = if isTopLevel then globalsAfterArgs else localEnv
                   in Right (builtinValue : stack, globalsAfterArgs, newLocalEnv, functionsAfterArgs, outputsAfterArgs)
                Nothing -> Left ("Name error: undefined function " ++ fname ++ " at " ++ showPos pos)

    callModuleMemberFunction args globalsNow functionsNow outputsNow =
      case args of
        ModuleValue moduleName _ : restArgs ->
          let memberFunctionName = modulePrefixFor (splitByDot moduleName) ++ fname
           in case Map.lookup memberFunctionName functionsNow of
                Just _ -> Just (callUserFunction memberFunctionName Nothing restArgs globalsNow functionsNow outputsNow)
                Nothing -> Nothing
        _ -> Nothing

    splitByDot text =
      case break (== '.') text of
        (segment, []) -> [segment]
        (segment, '.' : rest) -> segment : splitByDot rest
        _ -> [text]

    isBuiltinName name =
      case callBuiltin name [] pos of
        Just _ -> True
        Nothing -> False
