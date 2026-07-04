module PythonHS.VM.ExecuteCallValueFunction (executeCallValueFunction) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (FunctionRefValue, IntValue))
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.BindCallArguments (bindCallArguments)
import PythonHS.VM.BindDefaults (bindDefaults)
import PythonHS.VM.CollectFunctionGlobalDecls (collectFunctionGlobalDecls)
import PythonHS.VM.EnvState (EnvState (..))
import PythonHS.VM.EvaluateUserArgs (evaluateUserArgs)
import PythonHS.VM.ExceptionState (ExceptionState (..))
import PythonHS.VM.Instruction (Instruction)
import PythonHS.VM.LoopState (LoopState (..))
import PythonHS.VM.VMState (VMState (..))

executeCallValueFunction ::
  (VMState -> Either String VMState) ->
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
              let returnValue =
                    case vmStack finalState of
                      (value : _) -> value
                      [] -> IntValue 0
                  newLocalEnv = if isTopLevel then envGlobals (vmEnv finalState) else localEnv
              Right (returnValue : restStack, envGlobals (vmEnv finalState), newLocalEnv, envFunctions (vmEnv finalState), vmOutputs finalState)
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
