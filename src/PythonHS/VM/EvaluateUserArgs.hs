module PythonHS.VM.EvaluateUserArgs (evaluateUserArgs) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (DictValue, ListValue, NoneValue, StringValue), Value)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.Instruction (Instruction (ReturnTop), Instruction)

evaluateUserArgs ::
  ([Instruction] -> Int -> [Value] -> Map.Map String Value -> Map.Map String Value -> Map.Map String ([String], [(String, [Instruction])], [Instruction]) -> Set.Set String -> Map.Map Int [Value] -> Map.Map Int Int -> [Int] -> [String] -> Bool -> Either String (Maybe Value, Map.Map String Value, Map.Map String ([String], [(String, [Instruction])], [Instruction]), [String])) ->
  String ->
  Position ->
  Map.Map String Value ->
  [([Instruction], Maybe String, Position)] ->
  Map.Map String Value ->
  Map.Map String ([String], [(String, [Instruction])], [Instruction]) ->
  [String] ->
  Bool ->
  Set.Set String ->
  [Value] ->
  [(Maybe String, Position)] ->
  Either String ([Value], [(Maybe String, Position)], Map.Map String Value, Map.Map String ([String], [(String, [Instruction])], [Instruction]), [String])
evaluateUserArgs executeFn callName callPos currentLocalEnv remainingArgs currentGlobals currentFunctions currentOutputs seenKeywordArg seenKeywordNames accValues accKinds =
  case remainingArgs of
    [] -> Right (accValues, accKinds, currentGlobals, currentFunctions, currentOutputs)
    (argCode, argKind, argPos) : restArgs ->
      case argKind of
        Nothing ->
          if seenKeywordArg
            then Left ("Argument count mismatch when calling " ++ callName ++ " at " ++ showPos callPos)
            else do
              (argValue, globalsAfterArg, functionsAfterArg, outputsAfterArg) <-
                evalArgCode argCode currentGlobals currentFunctions currentOutputs
              evaluateUserArgs executeFn callName callPos currentLocalEnv restArgs globalsAfterArg functionsAfterArg outputsAfterArg False seenKeywordNames (accValues ++ [argValue]) (accKinds ++ [(Nothing, argPos)])
        Just argName
          | argName == starArgMarker -> do
              (argValue, globalsAfterArg, functionsAfterArg, outputsAfterArg) <-
                evalArgCode argCode currentGlobals currentFunctions currentOutputs
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
                        (accKinds ++ fmap (\_ -> (Nothing, argPos)) unpackedValues)
                _ -> Left ("Type error: * expects list at " ++ showPos argPos)
          | argName == kwStarArgMarker -> do
              (argValue, globalsAfterArg, functionsAfterArg, outputsAfterArg) <-
                evalArgCode argCode currentGlobals currentFunctions currentOutputs
              case argValue of
                DictValue keyValuePairs ->
                  appendKwPairs keyValuePairs argPos restArgs globalsAfterArg functionsAfterArg outputsAfterArg seenKeywordNames accValues accKinds
                _ -> Left ("Type error: ** expects dict at " ++ showPos argPos)
          | Set.member argName seenKeywordNames ->
              Left ("Argument error: duplicate keyword argument " ++ argName ++ " at " ++ showPos argPos)
          | otherwise -> do
              (argValue, globalsAfterArg, functionsAfterArg, outputsAfterArg) <-
                evalArgCode argCode currentGlobals currentFunctions currentOutputs
              let newSeenKeywordNames = Set.insert argName seenKeywordNames
              evaluateUserArgs executeFn callName callPos currentLocalEnv restArgs globalsAfterArg functionsAfterArg outputsAfterArg True newSeenKeywordNames (accValues ++ [argValue]) (accKinds ++ [(Just argName, argPos)])
  where
    starArgMarker = "__python_hs_star_arg__"
    kwStarArgMarker = "__python_hs_kwstar_arg__"

    evalArgCode argCode globalsNow functionsNow outputsNow = do
      (maybeArgValue, globalsAfterArg, functionsAfterArg, outputsAfterArg) <-
        executeFn (argCode ++ [ReturnTop]) 0 [] globalsNow currentLocalEnv functionsNow Set.empty Map.empty Map.empty [] outputsNow False
      let argValue =
            case maybeArgValue of
              Just value -> value
              Nothing -> NoneValue
      Right (argValue, globalsAfterArg, functionsAfterArg, outputsAfterArg)

    appendKwPairs keyValuePairs kwPos argsAfterKw globalsNow functionsNow outputsNow seenNames values kinds =
      case keyValuePairs of
        [] ->
          evaluateUserArgs executeFn callName callPos currentLocalEnv argsAfterKw globalsNow functionsNow outputsNow True seenNames values kinds
        (keyValue, value) : restPairs ->
          case keyValue of
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
            _ -> Left ("Type error: ** expects string keys at " ++ showPos kwPos)
