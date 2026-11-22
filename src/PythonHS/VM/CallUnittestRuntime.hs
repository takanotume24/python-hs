module PythonHS.VM.CallUnittestRuntime (callUnittestRuntime) where

import Data.Map.Strict qualified as Map
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.CallUnittestRuntimeConfig (CallUnittestRuntimeConfig (..))
import PythonHS.VM.CompiledCallArg (CompiledCallArg (..))
import PythonHS.VM.ExecuteCallValueFunction (executeCallValueFunction)
import PythonHS.VM.FunctionInfo (FunctionInfo)
import PythonHS.VM.Instruction (Instruction (PushConst))
import PythonHS.VM.MatchesExceptionClass (matchesExceptionClass)

-- | Handle unittest helpers that need to execute user-supplied callables.
--
-- The pure assertion helpers live in 'callUnittestBuiltin'. This module is
-- separate because invoking a callable requires the VM interpreter loop.
callUnittestRuntime :: CallUnittestRuntimeConfig -> Maybe (Either String ([Value], Map.Map String Value, Map.Map String Value, Map.Map String FunctionInfo, [String]))
callUnittestRuntime config =
  case name of
    "assertRaises" -> assertRaisesCallable
    "assertRaisesRegex" -> assertRaisesRegexCallable
    "addCleanup" -> Just registerCleanup
    "__python_hs_run_cleanups__" -> Just runCleanups
    _ -> Nothing
  where
    name = callUnittestRuntimeName config
    args = callUnittestRuntimeArgs config
    pos = callUnittestRuntimePos config
    execute = callUnittestRuntimeExecute config
    isTopLevel = callUnittestRuntimeIsTopLevel config
    globalsEnv = callUnittestRuntimeGlobals config
    localEnv = callUnittestRuntimeLocals config
    functionsEnv = callUnittestRuntimeFunctions config
    outputsEnv = callUnittestRuntimeOutputs config
    stack = callUnittestRuntimeStack config

    assertRaisesCallable =
      case args of
        _self : expected : func : restArgs
          | isCallable func ->
              Just
                ( case invoke func restArgs (stack, globalsEnv, localEnv, functionsEnv, outputsEnv) of
                    Left err
                      | matchesExceptionClass (expectedName expected) err ->
                          Right (NoneValue : stack, globalsEnv, localEnv, functionsEnv, outputsEnv)
                      | otherwise -> Left err
                    Right _ ->
                      Left
                        ( "AssertionError: expected "
                            ++ expectedName expected
                            ++ " to be raised at "
                            ++ showPos pos
                        )
                )
        _ -> Nothing

    assertRaisesRegexCallable =
      case args of
        _self : expected : _regex : func : restArgs
          | isCallable func ->
              Just
                ( case invoke func restArgs (stack, globalsEnv, localEnv, functionsEnv, outputsEnv) of
                    Left err
                      | matchesExceptionClass (expectedName expected) err ->
                          Right (NoneValue : stack, globalsEnv, localEnv, functionsEnv, outputsEnv)
                      | otherwise -> Left err
                    Right _ ->
                      Left
                        ( "AssertionError: expected "
                            ++ expectedName expected
                            ++ " to be raised at "
                            ++ showPos pos
                        )
                )
        _ -> Nothing

    registerCleanup =
      case args of
        _self : cleanupFunc : cleanupArgs ->
          let cleanupEntry = TupleValue {tupleValueItems = [cleanupFunc, ListValue {listValueItems = cleanupArgs}]}
              cleanups = case Map.lookup cleanupsName globalsEnv of
                Just (ListValue {listValueItems = existing}) -> existing
                _ -> []
              newGlobals = Map.insert cleanupsName (ListValue {listValueItems = cleanups ++ [cleanupEntry]}) globalsEnv
              newLocals = if isTopLevel then newGlobals else localEnv
           in Right (NoneValue : stack, newGlobals, newLocals, functionsEnv, outputsEnv)
        _ -> Left (countMismatch "addCleanup")

    runCleanups =
      case Map.lookup cleanupsName globalsEnv of
        Just (ListValue {listValueItems = cleanups}) ->
          case runAll (reverse cleanups) (stack, globalsEnv, localEnv, functionsEnv, outputsEnv) of
            Left err -> Left err
            Right (resultStack, newGlobals, newLocals, newFunctions, newOutputs) ->
              let clearedGlobals = Map.insert cleanupsName (ListValue {listValueItems = []}) newGlobals
                  clearedLocals = if isTopLevel then clearedGlobals else newLocals
               in Right (resultStack, clearedGlobals, clearedLocals, newFunctions, newOutputs)
        _ -> Right (NoneValue : stack, globalsEnv, localEnv, functionsEnv, outputsEnv)

    runAll [] (currentStack, currentGlobals, currentLocals, currentFunctions, currentOutputs) =
      Right (NoneValue : currentStack, currentGlobals, currentLocals, currentFunctions, currentOutputs)
    runAll (cleanup : rest) (currentStack, currentGlobals, currentLocals, currentFunctions, currentOutputs) =
      case cleanup of
        TupleValue {tupleValueItems = [cleanupFunc, ListValue {listValueItems = cleanupArgs}]} ->
          case invoke cleanupFunc cleanupArgs (currentStack, currentGlobals, currentLocals, currentFunctions, currentOutputs) of
            Left err -> Left err
            Right (resultStack, newGlobals, newLocals, newFunctions, newOutputs) ->
              runAll rest (drop 1 resultStack, newGlobals, newLocals, newFunctions, newOutputs)
        _ -> Left ("Runtime error: invalid cleanup entry at " ++ showPos pos)

    invoke funcValue argValues (currentStack, currentGlobals, currentLocals, currentFunctions, currentOutputs) =
      executeCallValueFunction
        execute
        isTopLevel
        (map valueToPushArg argValues)
        pos
        (funcValue : currentStack)
        currentGlobals
        currentLocals
        currentFunctions
        currentOutputs

    valueToPushArg value =
      CompiledCallArg
        { compiledCallArgCode = [PushConst value],
          compiledCallArgKeyword = Nothing,
          compiledCallArgPos = pos
        }

    expectedName value =
      case value of
        ClassValue {classValueName = className} -> className
        ExceptionValue {exceptionValueClass = className} -> className
        FunctionRefValue {functionRefValueName = functionName} -> functionName
        StringValue {stringValue = text} -> text
        _ -> "Exception"

    isCallable value =
      case value of
        FunctionRefValue {} -> True
        _ -> False

    cleanupsName = "__python_hs_cleanups__"

    countMismatch functionName =
      "Argument count mismatch when calling " ++ functionName ++ " at " ++ showPos pos
