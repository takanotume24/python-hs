module PythonHS.VM.ExecuteCallValueFunction (executeCallValueFunction) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (FunctionRefValue, IntValue), Value)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.BindCallArguments (bindCallArguments)
import PythonHS.VM.BindDefaults (bindDefaults)
import PythonHS.VM.CollectFunctionGlobalDecls (collectFunctionGlobalDecls)
import PythonHS.VM.EvaluateUserArgs (evaluateUserArgs)
import PythonHS.VM.Instruction (Instruction)

executeCallValueFunction ::
  ([Instruction] -> Int -> [Value] -> Map.Map String Value -> Map.Map String Value -> Map.Map String ([String], [(String, [Instruction])], [Instruction]) -> Set.Set String -> Map.Map Int [Value] -> Map.Map Int Int -> [Int] -> [String] -> Bool -> Either String (Maybe Value, Map.Map String Value, Map.Map String ([String], [(String, [Instruction])], [Instruction]), [String])) ->
  Bool ->
  [([Instruction], Maybe String, Position)] ->
  Position ->
  [Value] ->
  Map.Map String Value ->
  Map.Map String Value ->
  Map.Map String ([String], [(String, [Instruction])], [Instruction]) ->
  [String] ->
  Either String ([Value], Map.Map String Value, Map.Map String Value, Map.Map String ([String], [(String, [Instruction])], [Instruction]), [String])
executeCallValueFunction execute isTopLevel compiledArgs pos stack globalsEnv localEnv functions outputs =
  case stack of
    callableValue : restStack ->
      case callableValue of
        FunctionRefValue functionName capturedBindings ->
          case Map.lookup functionName functions of
            Nothing -> Left ("Name error: undefined function " ++ functionName ++ " at " ++ showPos pos)
            Just (params, defaultCodes, functionCode) -> do
              (argValues, argKinds, globalsAfterArgs, functionsAfterArgs, outputsAfterArgs) <-
                evaluateUserArgs execute functionName pos localEnv compiledArgs globalsEnv functions outputs False Set.empty [] []
              let (boundArgValues, boundArgKinds) = injectBoundSelf params capturedBindings argValues argKinds
              callLocals <- bindCallArguments functionName pos params boundArgValues boundArgKinds
              let capturedLocals = Map.fromList capturedBindings
                  mergedLocals = Map.union callLocals capturedLocals
              (functionLocals, globalsAfterDefaults, functionsAfterDefaults, outputsAfterDefaults) <-
                bindDefaults execute functionName pos params defaultCodes mergedLocals globalsAfterArgs functionsAfterArgs outputsAfterArgs
              let functionGlobalDecls = collectFunctionGlobalDecls functionCode
              (maybeValue, newGlobals, newFunctions, newOutputs) <-
                execute functionCode 0 [] globalsAfterDefaults functionLocals functionsAfterDefaults functionGlobalDecls Map.empty Map.empty [] outputsAfterDefaults False
              let returnValue =
                    case maybeValue of
                      Just value -> value
                      Nothing -> IntValue 0
                  newLocalEnv = if isTopLevel then newGlobals else localEnv
              Right (returnValue : restStack, newGlobals, newLocalEnv, newFunctions, newOutputs)
        _ -> Left ("Type error: callable expected at " ++ showPos pos)
    _ -> Left "VM runtime error: call requires callable on stack"
  where
    injectBoundSelf params capturedBindings argValues argKinds =
      case params of
        firstParam : _ ->
          case lookup "__python_hs_bound_self__" capturedBindings of
            Just boundSelf ->
              if any (\(maybeName, _) -> maybeName == Just firstParam) argKinds
                then (argValues, argKinds)
                else (boundSelf : argValues, (Nothing, pos) : argKinds)
            Nothing -> (argValues, argKinds)
        [] -> (argValues, argKinds)
