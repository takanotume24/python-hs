module PythonHS.VM.EvaluateBuiltinArgs (evaluateBuiltinArgs) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import PythonHS.Evaluator.Value (Value (NoneValue))
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.EnvState (EnvState (..))
import PythonHS.VM.ExceptionState (ExceptionState (..))
import PythonHS.VM.Instruction (Instruction (ReturnTop))
import PythonHS.VM.LoopState (LoopState (..))
import PythonHS.VM.VMState (VMState (..))

evaluateBuiltinArgs ::
  (VMState -> Either String VMState) ->
  Map.Map String Value ->
  [([Instruction], Maybe String, Position)] ->
  Map.Map String Value ->
  Map.Map String ([String], [(String, [Instruction])], [Instruction]) ->
  [String] ->
  [Value] ->
  Either String ([Value], Map.Map String Value, Map.Map String ([String], [(String, [Instruction])], [Instruction]), [String])
evaluateBuiltinArgs executeFn currentLocalEnv remainingArgs currentGlobals currentFunctions currentOutputs accValues =
  case remainingArgs of
    [] -> Right (accValues, currentGlobals, currentFunctions, currentOutputs)
    (argCode, _, _) : restArgs -> do
      (argValue, globalsAfterArg, functionsAfterArg, outputsAfterArg) <-
        evalArgCode argCode currentGlobals currentFunctions currentOutputs
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
