module PythonHS.VM.CallInstanceMethodString (callInstanceMethodString) where

import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Evaluator.ValueToOutput (valueToOutput)
import PythonHS.VM.CallInstanceMethodStringConfig (CallInstanceMethodStringConfig (..))
import PythonHS.VM.CallInstanceMethodStringResult (CallInstanceMethodStringResult (..))
import PythonHS.VM.ExecuteCallUserFunction (executeCallUserFunction)
import PythonHS.VM.FindMethodFunctionName (findMethodFunctionName)
import PythonHS.VM.MethodNamePair (MethodNamePair (..))

-- | Calls a user-defined @__str__@ (or @__repr__@ fallback) on an instance.
-- Returns Nothing when the value is not an instance or neither method exists.
callInstanceMethodString :: CallInstanceMethodStringConfig -> Maybe (Either String CallInstanceMethodStringResult)
callInstanceMethodString config =
  case callInstanceMethodStringInstance config of
    instanceValue@(InstanceValue {instanceValueClass = className}) ->
      case findStringMethod className of
        Just methodPair -> Just (invoke instanceValue methodPair)
        Nothing -> Nothing
    _ -> Nothing
  where
    globals = callInstanceMethodStringGlobals config
    locals = callInstanceMethodStringLocals config
    stack = callInstanceMethodStringStack config

    findStringMethod className =
      case findMethodFunctionName globals locals className "__str__" of
        Just methodPair -> Just methodPair
        Nothing -> findMethodFunctionName globals locals className "__repr__"

    invoke instanceValue methodPair =
      case
          executeCallUserFunction
            (callInstanceMethodStringExecute config)
            (callInstanceMethodStringIsTopLevel config)
            (callInstanceMethodStringPos config)
            stack
            locals
            (methodNamePairMangled methodPair)
            Nothing
            [instanceValue]
            globals
            (callInstanceMethodStringFunctions config)
            (callInstanceMethodStringOutputs config)
        of
        Left err -> Left err
        Right (returnedStack, newGlobals, newLocals, newFunctions, newOutputs) ->
          case returnedStack of
            [] -> Left "VM runtime error: __str__ did not return a value"
            (returnValue : _) ->
              let text =
                    case returnValue of
                      StringValue {stringValue = returnedText} -> returnedText
                      otherValue -> valueToOutput otherValue
               in Right
                    CallInstanceMethodStringResult
                      { callInstanceMethodStringResultStack = StringValue {stringValue = text} : stack,
                        callInstanceMethodStringResultGlobals = newGlobals,
                        callInstanceMethodStringResultLocals = newLocals,
                        callInstanceMethodStringResultFunctions = newFunctions,
                        callInstanceMethodStringResultOutputs = newOutputs
                      }
