module PythonHS.VM.ExecuteCreateInstance (executeCreateInstance) where

import Data.Map.Strict qualified as Map
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (InstanceValue))
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.BindCallArguments (bindCallArguments)
import PythonHS.VM.BindDefaults (bindDefaults)
import PythonHS.VM.CollectFunctionGlobalDecls (collectFunctionGlobalDecls)
import PythonHS.VM.EnvState (EnvState (..))
import PythonHS.VM.ExceptionState (ExceptionState (..))
import PythonHS.VM.FindMethodFunctionName (findMethodFunctionName)
import PythonHS.VM.Instruction (Instruction)
import PythonHS.VM.LoopState (LoopState (..))
import PythonHS.VM.VMState (VMState (..))

executeCreateInstance ::
  (VMState -> Either String VMState) ->
  Bool ->
  Position ->
  [Value] ->
  Map.Map String Value ->
  String ->
  [Value] ->
  Map.Map String Value ->
  Map.Map String ([String], [(String, [Instruction])], [Instruction]) ->
  [String] ->
  Either String ([Value], Map.Map String Value, Map.Map String Value, Map.Map String ([String], [(String, [Instruction])], [Instruction]), [String])
executeCreateInstance execute isTopLevel pos stack localEnv className args globalsNow functionsNow outputsNow =
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
                  callState =
                    VMState
                      { vmCode = initCode,
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
              let constructedInstance = case vmStack finalState of (instanceResult@(InstanceValue _ _) : _) -> instanceResult; _ -> instanceValue
                  newLocalEnv = if isTopLevel then envGlobals (vmEnv finalState) else localEnv
              Right (constructedInstance : stack, envGlobals (vmEnv finalState), newLocalEnv, envFunctions (vmEnv finalState), vmOutputs finalState)
        Nothing ->
          let newLocalEnv = if isTopLevel then globalsNow else localEnv
           in Right (instanceValue : stack, globalsNow, newLocalEnv, functionsNow, outputsNow)
