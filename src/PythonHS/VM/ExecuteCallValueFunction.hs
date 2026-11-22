module PythonHS.VM.ExecuteCallValueFunction (executeCallValueFunction) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (FunctionRefValue, IntValue))
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.BindCallArguments (bindCallArguments)
import PythonHS.VM.BindDefaults (bindDefaults)
import PythonHS.VM.CallBuiltin (callBuiltin)
import PythonHS.VM.CallBuiltinConfig (CallBuiltinConfig (..))
import PythonHS.VM.CollectFunctionGlobalDecls (collectFunctionGlobalDecls)
import PythonHS.VM.CompiledCallArg (CompiledCallArg (..))
import PythonHS.VM.EnvState (EnvState (..))
import PythonHS.VM.EvaluateUserArgs (evaluateUserArgs)
import PythonHS.VM.ExceptionState (ExceptionState (..))
import PythonHS.VM.FindInstanceMutation (findInstanceMutation)
import PythonHS.VM.FunctionInfo (FunctionInfo (..))
import PythonHS.VM.InstanceMutation (InstanceMutation (..))
import PythonHS.VM.Instruction (Instruction)
import PythonHS.VM.IsBuiltinCallableName (isBuiltinCallableName)
import PythonHS.VM.LoopState (LoopState (..))
import PythonHS.VM.PropagateInstanceMutation (propagateInstanceMutation)
import PythonHS.VM.VMState (VMState (..))

executeCallValueFunction ::
  (VMState -> Either String VMState) ->
  Bool ->
  [CompiledCallArg Instruction] ->
  Position ->
  [Value] ->
  Map.Map String Value ->
  Map.Map String Value ->
  Map.Map String FunctionInfo ->
  [String] ->
  Either String ([Value], Map.Map String Value, Map.Map String Value, Map.Map String FunctionInfo, [String])
executeCallValueFunction execute isTopLevel compiledArgs pos stack globalsEnv localEnv functions outputs =
  case stack of
    callableValue : restStack ->
      case callableValue of
        FunctionRefValue functionName capturedBindings ->
          case Map.lookup functionName functions of
            Nothing
              | isBuiltinCallableName functionName ->
                  callBuiltinAsValue functionName capturedBindings restStack
            Nothing -> Left ("Name error: undefined function " ++ functionName ++ " at " ++ showPos pos)
            Just (FunctionInfo params defaultCodes functionCode) -> do
              (argValues, argKinds, globalsAfterArgs, functionsAfterArgs, outputsAfterArgs) <-
                evaluateUserArgs execute functionName pos localEnv compiledArgs globalsEnv functions outputs False Set.empty [] []
              let (boundArgValues, boundArgKinds) = injectBoundSelf params capturedBindings argValues argKinds
              callLocals <- bindCallArguments functionName pos params boundArgValues boundArgKinds
              let capturedLocals = Map.fromList capturedBindings
                  mergedLocals = Map.union callLocals capturedLocals
              (functionLocals, globalsAfterDefaults, functionsAfterDefaults, outputsAfterDefaults) <-
                bindDefaults execute functionName pos params defaultCodes mergedLocals globalsAfterArgs functionsAfterArgs outputsAfterArgs
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
                        vmOutputs = outputsAfterDefaults,
                        vmTraceSteps = [],
                        vmTraceFailure = Nothing
                      }
              finalState <- execute callState
              let finalEnv = vmEnv finalState
                  mutatedSelf = findInstanceMutation params capturedBindings (envLocals finalEnv)
                  applyMutation environment =
                    case mutatedSelf of
                      Just InstanceMutation {instanceMutationOldValue = oldValue, instanceMutationNewValue = newValue} ->
                        propagateInstanceMutation oldValue newValue environment
                      Nothing -> environment
                  propagatedGlobals = applyMutation (envGlobals finalEnv)
                  propagatedLocals = applyMutation localEnv
                  returnValue =
                    case vmStack finalState of
                      (value : _) -> value
                      [] -> IntValue 0
                  newLocalEnv = if isTopLevel then propagatedGlobals else propagatedLocals
              Right (returnValue : restStack, propagatedGlobals, newLocalEnv, envFunctions finalEnv, vmOutputs finalState)
        _ -> Left ("Type error: callable expected at " ++ showPos pos)
    _ -> Left "VM runtime error: call requires callable on stack"
  where
    callBuiltinAsValue functionName capturedBindings restStack = do
      (argValues, _, globalsAfterArgs, functionsAfterArgs, outputsAfterArgs) <-
        evaluateUserArgs execute functionName pos localEnv compiledArgs globalsEnv functions outputs False Set.empty [] []
      let finalArgs = case lookup "__python_hs_bound_self__" capturedBindings of
            Just boundSelf -> boundSelf : argValues
            Nothing -> argValues
      case callBuiltin CallBuiltinConfig {callBuiltinName = functionName, callBuiltinArgs = finalArgs, callBuiltinPos = pos} of
        Just (Right builtinValue) ->
          let newLocalEnv = if isTopLevel then globalsAfterArgs else localEnv
           in Right (builtinValue : restStack, globalsAfterArgs, newLocalEnv, functionsAfterArgs, outputsAfterArgs)
        Just (Left err) -> Left err
        Nothing -> Left ("Name error: undefined function " ++ functionName ++ " at " ++ showPos pos)

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
