module PythonHS.VM.ExecuteCallFunction (executeCallFunction) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (ClassValue, FunctionRefValue, InstanceValue, IntValue, ModuleValue))
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.BindCallArguments (bindCallArguments)
import PythonHS.VM.BindDefaults (bindDefaults)
import PythonHS.VM.CallBuiltin (callBuiltin)
import PythonHS.VM.CallBuiltinConfig (CallBuiltinConfig (..))
import PythonHS.VM.CollectFunctionGlobalDecls (collectFunctionGlobalDecls)
import PythonHS.VM.EnvState (EnvState (..))
import PythonHS.VM.EvaluateBuiltinArgs (evaluateBuiltinArgs)
import PythonHS.VM.EvaluateUserArgs (evaluateUserArgs)
import PythonHS.VM.ExceptionState (ExceptionState (..))
import PythonHS.VM.ExecuteCallUserFunction (executeCallUserFunction)
import PythonHS.VM.ExecuteCallValueFunction (executeCallValueFunction)
import PythonHS.VM.ExecuteCreateInstance (executeCreateInstance)
import PythonHS.VM.FindMethodFunctionName (findMethodFunctionName)
import PythonHS.VM.FirstKeywordArg (firstKeywordArg)
import PythonHS.VM.Instruction (Instruction)
import PythonHS.VM.LookupName (lookupName)
import PythonHS.VM.LoopState (LoopState (..))
import PythonHS.VM.ModulePrefixFor (modulePrefixFor)
import PythonHS.VM.VMState (VMState (..))

executeCallFunction ::
  (VMState -> Either String VMState) ->
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
              callState =
                VMState
                  { vmCode = functionCode,
                    vmIp = 0,
                    vmStack = [],
                    vmEnv =
                      EnvState
                        { envGlobals = globalsAfterDefaults,
                          envLocals = functionLocals,
                          envFunctions = functionsAfterDefaults,
                          envGlobalDecls = functionGlobalDecls
                        },
                    vmLoop = LoopState {loopForStates = Map.empty, loopCounts = Map.empty},
                    vmException = ExceptionState {exceptionHandlers = [], exceptionOutputs = []},
                    vmIsTopLevel = False,
                    vmOutputs = outputsAfterDefaults
                  }
          finalState <- execute callState
          let returnValue = case vmStack finalState of (value : _) -> value; [] -> IntValue 0
              newLocalEnv = if isTopLevel then envGlobals (vmEnv finalState) else localEnv
          Right (returnValue : stack, envGlobals (vmEnv finalState), newLocalEnv, envFunctions (vmEnv finalState), vmOutputs finalState)
        Nothing ->
          case firstKeywordArg compiledArgs of
            Just (_, argPos)
              | isBuiltinName fname ->
                  Left ("Argument error: keyword arguments are not supported for builtin " ++ fname ++ " at " ++ showPos argPos)
            _ ->
              case evaluateBuiltinArgs execute localEnv compiledArgs globalsEnv functions outputs [] of
                Left err -> Left err
                Right (args, globalsAfterArgs, functionsAfterArgs, outputsAfterArgs) ->
                  case lookupName fname localEnv globalsEnv of
                    Just (ClassValue className _ _) ->
                      executeCreateInstance execute isTopLevel pos stack localEnv className args globalsAfterArgs functionsAfterArgs outputsAfterArgs
                    _ ->
                      case args of
                        ClassValue className _ _ : methodArgs@(InstanceValue _ _ : _) ->
                          case findMethodFunctionName globalsAfterArgs localEnv className fname of
                            Just methodFunctionName ->
                              executeCallUserFunction execute isTopLevel pos stack localEnv methodFunctionName Nothing methodArgs globalsAfterArgs functionsAfterArgs outputsAfterArgs
                            Nothing -> callBuiltinOrFail args globalsAfterArgs functionsAfterArgs outputsAfterArgs
                        InstanceValue className _ : _ ->
                          case findMethodFunctionName globalsAfterArgs localEnv className fname of
                            Just methodFunctionName ->
                              executeCallUserFunction execute isTopLevel pos stack localEnv methodFunctionName Nothing args globalsAfterArgs functionsAfterArgs outputsAfterArgs
                            Nothing -> callBuiltinOrFail args globalsAfterArgs functionsAfterArgs outputsAfterArgs
                        _ -> callBuiltinOrFail args globalsAfterArgs functionsAfterArgs outputsAfterArgs
  where
    callBuiltinOrFail args globalsAfterArgs functionsAfterArgs outputsAfterArgs =
      case callModuleMemberFunction args globalsAfterArgs functionsAfterArgs outputsAfterArgs of
        Just result -> result
        Nothing ->
          case firstKeywordArg compiledArgs of
            Just (_, argPos)
              | isBuiltinName fname ->
                  Left ("Argument error: keyword arguments are not supported for builtin " ++ fname ++ " at " ++ showPos argPos)
            Nothing ->
              case callBuiltin CallBuiltinConfig {callBuiltinName = fname, callBuiltinArgs = args, callBuiltinPos = pos} of
                Just (Left err) -> Left err
                Just (Right builtinValue) ->
                  let newLocalEnv = if isTopLevel then globalsAfterArgs else localEnv
                   in Right (builtinValue : stack, globalsAfterArgs, newLocalEnv, functionsAfterArgs, outputsAfterArgs)
                Nothing -> Left ("Name error: undefined function " ++ fname ++ " at " ++ showPos pos)
            _ ->
              case callBuiltin CallBuiltinConfig {callBuiltinName = fname, callBuiltinArgs = args, callBuiltinPos = pos} of
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
                Just _ -> Just (executeCallUserFunction execute isTopLevel pos stack localEnv memberFunctionName Nothing restArgs globalsNow functionsNow outputsNow)
                Nothing -> Nothing
        _ -> Nothing

    splitByDot text =
      case break (== '.') text of
        (segment, []) -> [segment]
        (segment, '.' : rest) -> segment : splitByDot rest
        _ -> [text]

    isBuiltinName name =
      case callBuiltin CallBuiltinConfig {callBuiltinName = name, callBuiltinArgs = [], callBuiltinPos = pos} of
        Just _ -> True
        Nothing -> False
