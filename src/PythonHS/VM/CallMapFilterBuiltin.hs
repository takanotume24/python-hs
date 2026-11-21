module PythonHS.VM.CallMapFilterBuiltin (callMapFilterBuiltin) where

import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.CallMapFilterBuiltinConfig (CallMapFilterBuiltinConfig (..))
import PythonHS.VM.CallMapFilterBuiltinResult (CallMapFilterBuiltinResult (..))
import PythonHS.VM.CompiledCallArg (CompiledCallArg (..))
import PythonHS.VM.ExecuteCallValueFunction (executeCallValueFunction)
import PythonHS.VM.Instruction (Instruction (PushConst))
import PythonHS.VM.IsTruthy (isTruthy)
import PythonHS.VM.IterableItems (iterableItems)

callMapFilterBuiltin :: CallMapFilterBuiltinConfig -> Maybe (Either String CallMapFilterBuiltinResult)
callMapFilterBuiltin config =
  case name of
    "map" -> Just (evalMapFilter False)
    "filter" -> Just (evalMapFilter True)
    _ -> Nothing
  where
    name = callMapFilterBuiltinName config
    args = callMapFilterBuiltinArgs config
    pos = callMapFilterBuiltinPos config

    evalMapFilter isFilter =
      case args of
        [functionValue, iterableValue] ->
          case iterableItems iterableValue of
            Nothing -> Left ("Type error: " ++ name ++ " expects iterable at " ++ showPos pos)
            Just items -> applyToItems isFilter functionValue items
        _ -> Left ("Argument count mismatch when calling " ++ name ++ " at " ++ showPos pos)

    applyToItems isFilter functionValue items =
      go
        items
        []
        (callMapFilterBuiltinGlobals config)
        (callMapFilterBuiltinLocals config)
        (callMapFilterBuiltinFunctions config)
        (callMapFilterBuiltinOutputs config)
      where
        go [] acc globalsNow localsNow functionsNow outputsNow =
          Right
            CallMapFilterBuiltinResult
              { callMapFilterBuiltinResultStack = ListValue {listValueItems = reverse acc} : callMapFilterBuiltinStack config,
                callMapFilterBuiltinResultGlobals = globalsNow,
                callMapFilterBuiltinResultLocals = finalLocals globalsNow localsNow,
                callMapFilterBuiltinResultFunctions = functionsNow,
                callMapFilterBuiltinResultOutputs = outputsNow
              }
        go (item : restItems) acc globalsNow localsNow functionsNow outputsNow = do
          (resultValue, globalsAfter, localsAfter, functionsAfter, outputsAfter) <-
            callOne functionValue item globalsNow localsNow functionsNow outputsNow
          let nextAcc =
                if isFilter
                  then (if isTruthy resultValue then item : acc else acc)
                  else resultValue : acc
          go restItems nextAcc globalsAfter localsAfter functionsAfter outputsAfter

    finalLocals globalsNow localsNow =
      if callMapFilterBuiltinIsTopLevel config then globalsNow else localsNow

    callOne functionValue item globalsNow localsNow functionsNow outputsNow =
      case executeCallValueFunction
        (callMapFilterBuiltinExecute config)
        (callMapFilterBuiltinIsTopLevel config)
        [CompiledCallArg {compiledCallArgCode = [PushConst item], compiledCallArgKeyword = Nothing, compiledCallArgPos = pos}]
        pos
        [functionValue]
        globalsNow
        localsNow
        functionsNow
        outputsNow of
        Left err -> Left err
        Right (resultingStack, globalsAfter, localsAfter, functionsAfter, outputsAfter) ->
          case resultingStack of
            resultValue : _ ->
              Right (resultValue, globalsAfter, localsAfter, functionsAfter, outputsAfter)
            [] -> Left ("VM runtime error: " ++ name ++ " callback did not return a value")
