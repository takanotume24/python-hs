module PythonHS.VM.ExecuteCallUserFunction (executeCallUserFunction) where

import Data.Map.Strict qualified as Map
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (IntValue))
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.BindCallArguments (bindCallArguments)
import PythonHS.VM.BindDefaults (bindDefaults)
import PythonHS.VM.CollectFunctionGlobalDecls (collectFunctionGlobalDecls)
import PythonHS.VM.EnvState (EnvState (..))
import PythonHS.VM.ExceptionState (ExceptionState (..))
import PythonHS.VM.Instruction (Instruction)
import PythonHS.VM.LoopState (LoopState (..))
import PythonHS.VM.VMState (VMState (..))

executeCallUserFunction ::
  (VMState -> Either String VMState) ->
  Bool ->
  Position ->
  [Value] ->
  Map.Map String Value ->
  String ->
  Maybe [(String, Value)] ->
  [Value] ->
  Map.Map String Value ->
  Map.Map String ([String], [(String, [Instruction])], [Instruction]) ->
  [String] ->
  Either String ([Value], Map.Map String Value, Map.Map String Value, Map.Map String ([String], [(String, [Instruction])], [Instruction]), [String])
executeCallUserFunction execute isTopLevel pos stack localEnv targetName maybeCapturedBindings args globalsNow functionsNow outputsNow =
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
