module PythonHS.VM.CallIteratorBuiltin (callIteratorBuiltin) where

import Data.List (isPrefixOf)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.CallBoundMethod (callBoundMethod)
import PythonHS.VM.CallBoundMethodConfig (CallBoundMethodConfig (..))
import PythonHS.VM.CallBoundMethodResult (CallBoundMethodResult (..))
import PythonHS.VM.CallIteratorBuiltinConfig (CallIteratorBuiltinConfig (..))
import PythonHS.VM.CallIteratorBuiltinResult (CallIteratorBuiltinResult (..))
import PythonHS.VM.ToIterableValues (toIterableValues)
import PythonHS.VM.ToIterableValuesConfig (ToIterableValuesConfig (..))
import PythonHS.VM.ToIterableValuesResult (ToIterableValuesResult (..))

callIteratorBuiltin :: CallIteratorBuiltinConfig -> Maybe (Either String CallIteratorBuiltinResult)
callIteratorBuiltin config =
  case callIteratorBuiltinName config of
    "next" -> Just evalNext
    "iter" -> Just evalIter
    "list" -> evalConstructor listItems
    "tuple" -> evalConstructor tupleItems
    _ -> Nothing
  where
    name = callIteratorBuiltinName config
    args = callIteratorBuiltinArgs config
    pos = callIteratorBuiltinPos config
    isTopLevel = callIteratorBuiltinIsTopLevel config

    evalNext =
      case args of
        [instanceValue@InstanceValue {}] -> advance instanceValue Nothing
        [instanceValue@InstanceValue {}, defaultValue] -> advance instanceValue (Just defaultValue)
        [otherValue] -> Left (notIteratorMessage otherValue)
        [otherValue, _] -> Left (notIteratorMessage otherValue)
        _ -> Left (countMismatch name)

    advance instanceValue maybeDefault =
      case callBoundMethod (nextConfig instanceValue) of
        Left err
          | isStopIteration err ->
              case maybeDefault of
                Just defaultValue -> Right (pushValue defaultValue globals locals functions outputs)
                Nothing -> Left err
          | otherwise -> Left err
        Right result ->
          Right
            ( pushValue
                (callBoundMethodResultValue result)
                (callBoundMethodResultGlobals result)
                (callBoundMethodResultLocals result)
                (callBoundMethodResultFunctions result)
                (callBoundMethodResultOutputs result)
            )

    nextConfig instanceValue =
      CallBoundMethodConfig
        { callBoundMethodExecute = callIteratorBuiltinExecute config,
          callBoundMethodIsTopLevel = isTopLevel,
          callBoundMethodPos = pos,
          callBoundMethodInstance = instanceValue,
          callBoundMethodName = "__next__",
          callBoundMethodMissingMessage = notIteratorMessage instanceValue,
          callBoundMethodGlobals = globals,
          callBoundMethodLocals = locals,
          callBoundMethodFunctions = functions,
          callBoundMethodOutputs = outputs
        }

    evalIter =
      case args of
        [instanceValue@InstanceValue {}] -> callIter instanceValue
        [otherValue] ->
          case otherValue of
            ListValue {} -> Right (pushValue otherValue globals locals functions outputs)
            TupleValue {} -> Right (pushValue otherValue globals locals functions outputs)
            DictValue {} -> Right (pushValue otherValue globals locals functions outputs)
            IntValue {} -> Right (pushValue otherValue globals locals functions outputs)
            _ -> Left ("Type error: " ++ typeNameOf otherValue ++ " is not iterable at " ++ showPos pos)
        _ -> Left (countMismatch name)

    callIter instanceValue =
      case callBoundMethod (iterConfig instanceValue) of
        Left err -> Left err
        Right result ->
          Right
            ( pushValue
                (callBoundMethodResultValue result)
                (callBoundMethodResultGlobals result)
                (callBoundMethodResultLocals result)
                (callBoundMethodResultFunctions result)
                (callBoundMethodResultOutputs result)
            )

    iterConfig instanceValue =
      CallBoundMethodConfig
        { callBoundMethodExecute = callIteratorBuiltinExecute config,
          callBoundMethodIsTopLevel = isTopLevel,
          callBoundMethodPos = pos,
          callBoundMethodInstance = instanceValue,
          callBoundMethodName = "__iter__",
          callBoundMethodMissingMessage = "Type error: " ++ typeNameOf instanceValue ++ " is not iterable at " ++ showPos pos,
          callBoundMethodGlobals = globals,
          callBoundMethodLocals = locals,
          callBoundMethodFunctions = functions,
          callBoundMethodOutputs = outputs
        }

    evalConstructor wrapItems =
      case args of
        [instanceValue@InstanceValue {}] ->
          Just
            ( case toIterableValues (iterableConfig instanceValue) of
                Left err -> Left err
                Right result ->
                  Right
                    ( pushValue
                        (wrapItems (toIterableValuesResultValues result))
                        (toIterableValuesResultGlobals result)
                        (toIterableValuesResultLocals result)
                        (toIterableValuesResultFunctions result)
                        (toIterableValuesResultOutputs result)
                    )
            )
        _ -> Nothing

    iterableConfig instanceValue =
      ToIterableValuesConfig
        { toIterableValuesExecute = callIteratorBuiltinExecute config,
          toIterableValuesIsTopLevel = isTopLevel,
          toIterableValuesValue = instanceValue,
          toIterableValuesPos = pos,
          toIterableValuesGlobals = globals,
          toIterableValuesLocals = locals,
          toIterableValuesFunctions = functions,
          toIterableValuesOutputs = outputs
        }

    pushValue resultValue globalsNow localsNow functionsNow outputsNow =
      CallIteratorBuiltinResult
        { callIteratorBuiltinResultStack = resultValue : callIteratorBuiltinStack config,
          callIteratorBuiltinResultGlobals = globalsNow,
          callIteratorBuiltinResultLocals = if isTopLevel then globalsNow else localsNow,
          callIteratorBuiltinResultFunctions = functionsNow,
          callIteratorBuiltinResultOutputs = outputsNow
        }

    listItems values = ListValue {listValueItems = values}
    tupleItems values = TupleValue {tupleValueItems = values}

    notIteratorMessage iterableValue =
      "Type error: " ++ typeNameOf iterableValue ++ " is not an iterator at " ++ showPos pos

    countMismatch functionName =
      "Argument count mismatch when calling " ++ functionName ++ " at " ++ showPos pos

    isStopIteration message = "StopIteration" `isPrefixOf` message

    typeNameOf typeValue =
      case typeValue of
        IntValue {} -> "int"
        FloatValue {} -> "float"
        StringValue {} -> "str"
        NoneValue -> "NoneType"
        ListValue {} -> "list"
        TupleValue {} -> "tuple"
        DictValue {} -> "dict"
        ModuleValue {} -> "module"
        FunctionRefValue {} -> "function"
        ClassValue {} -> "type"
        InstanceValue {instanceValueClass = className} -> className
        ExceptionValue {exceptionValueClass = className} -> className
        BreakValue -> "break"
        ContinueValue -> "continue"
        UnboundLocalValue {} -> "UnboundLocal"

    globals = callIteratorBuiltinGlobals config
    locals = callIteratorBuiltinLocals config
    functions = callIteratorBuiltinFunctions config
    outputs = callIteratorBuiltinOutputs config
