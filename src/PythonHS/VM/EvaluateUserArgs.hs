module PythonHS.VM.EvaluateUserArgs (evaluateUserArgs) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import PythonHS.Evaluator.DictPair (DictPair (..))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (DictValue, ListValue, NoneValue, StringValue))
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.CompiledCallArg (CompiledCallArg (..))
import PythonHS.VM.EnvState (EnvState (..))
import PythonHS.VM.ExceptionState (ExceptionState (..))
import PythonHS.VM.FunctionInfo (FunctionInfo)
import PythonHS.VM.Instruction (Instruction (ReturnTop))
import PythonHS.VM.LoopState (LoopState (..))
import PythonHS.VM.VMState (VMState (..))

evaluateUserArgs ::
  (VMState -> Either String VMState) ->
  String ->
  Position ->
  Map.Map String Value ->
  [CompiledCallArg Instruction] ->
  Map.Map String Value ->
  Map.Map String FunctionInfo ->
  [String] ->
  Bool ->
  Set.Set String ->
  [Value] ->
  [(Maybe String, Position)] ->
  Either String ([Value], [(Maybe String, Position)], Map.Map String Value, Map.Map String FunctionInfo, [String])
evaluateUserArgs executeFn callName callPos currentLocalEnv remainingArgs currentGlobals currentFunctions currentOutputs seenKeywordArg seenKeywordNames accValues accKinds =
  case remainingArgs of
    [] -> Right (accValues, accKinds, currentGlobals, currentFunctions, currentOutputs)
    arg : restArgs ->
      case compiledCallArgKeyword arg of
        Nothing ->
          if seenKeywordArg
            then Left ("Argument count mismatch when calling " ++ callName ++ " at " ++ showPos callPos)
            else do
              (argValue, globalsAfterArg, functionsAfterArg, outputsAfterArg) <-
                evalArgCode (compiledCallArgCode arg) currentGlobals currentFunctions currentOutputs
              evaluateUserArgs executeFn callName callPos currentLocalEnv restArgs globalsAfterArg functionsAfterArg outputsAfterArg False seenKeywordNames (accValues ++ [argValue]) (accKinds ++ [(Nothing, compiledCallArgPos arg)])
        Just argName
          | argName == starArgMarker -> do
              (argValue, globalsAfterArg, functionsAfterArg, outputsAfterArg) <-
                evalArgCode (compiledCallArgCode arg) currentGlobals currentFunctions currentOutputs
              case argValue of
                ListValue unpackedValues ->
                  if seenKeywordArg
                    then Left ("Argument count mismatch when calling " ++ callName ++ " at " ++ showPos callPos)
                    else
                      evaluateUserArgs
                        executeFn
                        callName
                        callPos
                        currentLocalEnv
                        restArgs
                        globalsAfterArg
                        functionsAfterArg
                        outputsAfterArg
                        False
                        seenKeywordNames
                        (accValues ++ unpackedValues)
                        (accKinds ++ fmap (\_ -> (Nothing, compiledCallArgPos arg)) unpackedValues)
                _ -> Left ("Type error: * expects list at " ++ showPos (compiledCallArgPos arg))
          | argName == kwStarArgMarker -> do
              (argValue, globalsAfterArg, functionsAfterArg, outputsAfterArg) <-
                evalArgCode (compiledCallArgCode arg) currentGlobals currentFunctions currentOutputs
              case argValue of
                DictValue keyValuePairs ->
                  appendKwPairs keyValuePairs (compiledCallArgPos arg) restArgs globalsAfterArg functionsAfterArg outputsAfterArg seenKeywordNames accValues accKinds
                _ -> Left ("Type error: ** expects dict at " ++ showPos (compiledCallArgPos arg))
          | Set.member argName seenKeywordNames ->
              Left ("Argument error: duplicate keyword argument " ++ argName ++ " at " ++ showPos (compiledCallArgPos arg))
          | otherwise -> do
              (argValue, globalsAfterArg, functionsAfterArg, outputsAfterArg) <-
                evalArgCode (compiledCallArgCode arg) currentGlobals currentFunctions currentOutputs
              let newSeenKeywordNames = Set.insert argName seenKeywordNames
              evaluateUserArgs executeFn callName callPos currentLocalEnv restArgs globalsAfterArg functionsAfterArg outputsAfterArg True newSeenKeywordNames (accValues ++ [argValue]) (accKinds ++ [(Just argName, compiledCallArgPos arg)])
  where
    starArgMarker = "__python_hs_star_arg__"
    kwStarArgMarker = "__python_hs_kwstar_arg__"

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

    appendKwPairs keyValuePairs kwPos argsAfterKw globalsNow functionsNow outputsNow seenNames values kinds =
      case keyValuePairs of
        [] ->
          evaluateUserArgs executeFn callName callPos currentLocalEnv argsAfterKw globalsNow functionsNow outputsNow True seenNames values kinds
        pair : restPairs ->
          let keyValue = dictPairKey pair
              value = dictPairValue pair
           in case keyValue of
                StringValue argName ->
                  if Set.member argName seenNames
                    then Left ("Argument error: duplicate keyword argument " ++ argName ++ " at " ++ showPos kwPos)
                    else
                      appendKwPairs
                        restPairs
                        kwPos
                        argsAfterKw
                        globalsNow
                        functionsNow
                        outputsNow
                        (Set.insert argName seenNames)
                        (values ++ [value])
                        (kinds ++ [(Just argName, kwPos)])
                _ -> Left ("Type error: keyword argument keys must be strings at " ++ showPos kwPos)
