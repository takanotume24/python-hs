module PythonHS.VM.EvaluateBuiltinArgs (evaluateBuiltinArgs) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import PythonHS.Evaluator.Value (Value (NoneValue))
import PythonHS.VM.CompiledCallArg (CompiledCallArg (..))
import PythonHS.VM.EnvState (EnvState (..))
import PythonHS.VM.ExceptionState (ExceptionState (..))
import PythonHS.VM.FunctionInfo (FunctionInfo)
import PythonHS.VM.Instruction (Instruction (ReturnTop))
import PythonHS.VM.LoopState (LoopState (..))
import PythonHS.VM.VMState (VMState (..))

evaluateBuiltinArgs ::
  (VMState -> Either String VMState) ->
  Map.Map String Value ->
  [CompiledCallArg Instruction] ->
  Map.Map String Value ->
  Map.Map String FunctionInfo ->
  [String] ->
  [Value] ->
  Either String ([Value], Map.Map String Value, Map.Map String FunctionInfo, [String])
evaluateBuiltinArgs executeFn currentLocalEnv remainingArgs currentGlobals currentFunctions currentOutputs accValues =
  case remainingArgs of
    [] -> Right (accValues, currentGlobals, currentFunctions, currentOutputs)
    arg : restArgs -> do
      (argValue, globalsAfterArg, functionsAfterArg, outputsAfterArg) <-
        evalArgCode (compiledCallArgCode arg) currentGlobals currentFunctions currentOutputs
      evaluateBuiltinArgs executeFn currentLocalEnv restArgs globalsAfterArg functionsAfterArg outputsAfterArg (accValues ++ [argValue])
  where
    evalArgCode argCode globalsNow functionsNow outputsNow = do
      let argState =
            VMState
              { vmCode = argCode ++ [ReturnTop],
                vmIp = 0,
                vmStack = [],
                vmEnv =
                  EnvState
                    { envGlobals = globalsNow,
                      envLocals = currentLocalEnv,
                      envFunctions = functionsNow,
                      envGlobalDecls = Set.empty
                    },
                vmLoop = LoopState {loopForStates = Map.empty, loopCounts = Map.empty},
                vmException = ExceptionState {exceptionHandlers = [], exceptionOutputs = []},
                vmIsTopLevel = False,
                vmOutputs = outputsNow
              }
      finalState <- executeFn argState
      let argValue =
            case vmStack finalState of
              (value : _) -> value
              [] -> NoneValue
      Right (argValue, envGlobals (vmEnv finalState), envFunctions (vmEnv finalState), vmOutputs finalState)
