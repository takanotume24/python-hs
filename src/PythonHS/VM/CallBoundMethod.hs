module PythonHS.VM.CallBoundMethod (callBoundMethod) where

import PythonHS.Evaluator.Value (Value (InstanceValue, instanceValueClass))
import PythonHS.VM.CallBoundMethodConfig (CallBoundMethodConfig (..))
import PythonHS.VM.CallBoundMethodResult (CallBoundMethodResult (..))
import PythonHS.VM.ExecuteCallUserFunction (executeCallUserFunction)
import PythonHS.VM.FindMethodFunctionName (findMethodFunctionName)
import PythonHS.VM.MethodContextBindings (methodContextBindings)
import PythonHS.VM.MethodKind (MethodKind (InstanceMethod))
import PythonHS.VM.MethodNamePair (MethodNamePair (..))

-- | Call an instance method with the receiver bound as @self@.
callBoundMethod :: CallBoundMethodConfig -> Either String CallBoundMethodResult
callBoundMethod config =
  case callBoundMethodInstance config of
    instanceValue@InstanceValue {instanceValueClass = className} ->
      case findMethodFunctionName globals locals className methodName of
        Nothing -> Left missingMessage
        Just methodPair ->
          case methodNamePairKind methodPair of
            InstanceMethod -> do
              (stack, globalsAfter, localsAfter, functionsAfter, outputsAfter) <-
                executeCallUserFunction
                  execute
                  isTopLevel
                  pos
                  []
                  locals
                  (methodNamePairMangled methodPair)
                  (Just (methodContextBindings className instanceValue ++ [("__python_hs_bound_self__", instanceValue)]))
                  [instanceValue]
                  globals
                  functions
                  outputs
              case stack of
                value : _ ->
                  Right
                    CallBoundMethodResult
                      { callBoundMethodResultValue = value,
                        callBoundMethodResultGlobals = globalsAfter,
                        callBoundMethodResultLocals = localsAfter,
                        callBoundMethodResultFunctions = functionsAfter,
                        callBoundMethodResultOutputs = outputsAfter
                      }
                [] -> Left "VM runtime error: method call did not produce a value"
            _ -> Left missingMessage
    _ -> Left missingMessage
  where
    execute = callBoundMethodExecute config
    isTopLevel = callBoundMethodIsTopLevel config
    pos = callBoundMethodPos config
    methodName = callBoundMethodName config
    missingMessage = callBoundMethodMissingMessage config
    globals = callBoundMethodGlobals config
    locals = callBoundMethodLocals config
    functions = callBoundMethodFunctions config
    outputs = callBoundMethodOutputs config
