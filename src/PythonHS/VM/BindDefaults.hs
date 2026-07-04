module PythonHS.VM.BindDefaults (bindDefaults) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (NoneValue))
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.EnvState (EnvState (..))
import PythonHS.VM.ExceptionState (ExceptionState (..))
import PythonHS.VM.Instruction (Instruction)
import PythonHS.VM.LoopState (LoopState (..))
import PythonHS.VM.VMState (VMState (..))

bindDefaults ::
  (VMState -> Either String VMState) ->
  String ->
  Position ->
  [String] ->
  [(String, [Instruction])] ->
  Map.Map String Value ->
  Map.Map String Value ->
  Map.Map String ([String], [(String, [Instruction])], [Instruction]) ->
  [String] ->
  Either String (Map.Map String Value, Map.Map String Value, Map.Map String ([String], [(String, [Instruction])], [Instruction]), [String])
bindDefaults execute fname pos params defaultCodes initialLocals globalsNow functionsNow outputsNow =
  fill params initialLocals globalsNow functionsNow outputsNow
  where
    defaultMap = Map.fromList (fmap (\(name, code) -> (canonicalName name, code)) defaultCodes)

    fill remainingParams currentLocals currentGlobals currentFunctions currentOutputs =
      case remainingParams of
        [] -> Right (currentLocals, currentGlobals, currentFunctions, currentOutputs)
        paramName : restParams ->
          case Map.lookup (canonicalName paramName) currentLocals of
            Just _ -> fill restParams currentLocals currentGlobals currentFunctions currentOutputs
            Nothing ->
              case Map.lookup (canonicalName paramName) defaultMap of
                Nothing -> Left ("Argument count mismatch when calling " ++ fname ++ " at " ++ showPos pos)
                Just defaultCode -> do
                  let defaultState =
                        VMState
                          { vmCode = defaultCode,
                            vmIp = 0,
                            vmStack = [],
                            vmEnv =
                              EnvState
                                { envGlobals = currentGlobals,
                                  envLocals = currentLocals,
                                  envFunctions = currentFunctions,
                                  envGlobalDecls = Set.empty
                                },
                            vmLoop = LoopState {loopForStates = Map.empty, loopCounts = Map.empty},
                            vmException = ExceptionState {exceptionHandlers = [], exceptionOutputs = []},
                            vmIsTopLevel = False,
                            vmOutputs = currentOutputs
                          }
                  finalState <- execute defaultState
                  let defaultValue =
                        case vmStack finalState of
                          (value : _) -> value
                          [] -> NoneValue
                      newLocals = Map.insert (canonicalName paramName) defaultValue currentLocals
                  fill restParams newLocals (envGlobals (vmEnv finalState)) (envFunctions (vmEnv finalState)) (vmOutputs finalState)

    canonicalName ('*' : '*' : rest) = rest
    canonicalName ('*' : rest) = rest
    canonicalName name = name
