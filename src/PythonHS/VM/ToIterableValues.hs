module PythonHS.VM.ToIterableValues (toIterableValues) where

import Data.List (isPrefixOf)
import Data.Map.Strict qualified as Map
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.CallBoundMethod (callBoundMethod)
import PythonHS.VM.CallBoundMethodConfig (CallBoundMethodConfig (..))
import PythonHS.VM.CallBoundMethodResult (CallBoundMethodResult (..))
import PythonHS.VM.ToForIterable (toForIterable)
import PythonHS.VM.ToForIterableConfig (ToForIterableConfig (..))
import PythonHS.VM.ToIterableValuesConfig (ToIterableValuesConfig (..))
import PythonHS.VM.ToIterableValuesResult (ToIterableValuesResult (..))

-- | Materialize any iterable, including instances that implement the
-- @__iter__@ / @__next__@ protocol.
toIterableValues :: ToIterableValuesConfig -> Either String ToIterableValuesResult
toIterableValues config =
  case value of
    InstanceValue {instanceValueClass = className} -> iterateInstance className
    _ -> plainValues value globals locals functions outputs
  where
    value = toIterableValuesValue config
    pos = toIterableValuesPos config
    globals = toIterableValuesGlobals config
    locals = toIterableValuesLocals config
    functions = toIterableValuesFunctions config
    outputs = toIterableValuesOutputs config
    hiddenIteratorName = "__python_hs_iterator__"

    plainValues iterableValue currentGlobals currentLocals currentFunctions currentOutputs = do
      values <- toForIterable ToForIterableConfig {toForIterableValue = iterableValue, toForIterablePos = pos}
      Right
        ToIterableValuesResult
          { toIterableValuesResultValues = values,
            toIterableValuesResultGlobals = currentGlobals,
            toIterableValuesResultLocals = currentLocals,
            toIterableValuesResultFunctions = currentFunctions,
            toIterableValuesResultOutputs = currentOutputs
          }

    iterateInstance className = do
      iterResult <- callBoundMethod (iterMethodConfig globals locals functions outputs value className)
      let iterator = callBoundMethodResultValue iterResult
      case iterator of
        InstanceValue {} ->
          collect
            []
            (Map.insert hiddenIteratorName iterator (callBoundMethodResultGlobals iterResult))
            (callBoundMethodResultLocals iterResult)
            (callBoundMethodResultFunctions iterResult)
            (callBoundMethodResultOutputs iterResult)
        _ ->
          plainValues
            iterator
            (callBoundMethodResultGlobals iterResult)
            (callBoundMethodResultLocals iterResult)
            (callBoundMethodResultFunctions iterResult)
            (callBoundMethodResultOutputs iterResult)

    collect acc globalsNow localsNow functionsNow outputsNow =
      case Map.lookup hiddenIteratorName globalsNow of
        Nothing -> Left "VM runtime error: iterator state was lost"
        Just iteratorNow ->
          case callBoundMethod (nextMethodConfig globalsNow localsNow functionsNow outputsNow iteratorNow) of
            Left err
              | isStopIteration err ->
                  Right
                    ToIterableValuesResult
                      { toIterableValuesResultValues = reverse acc,
                        toIterableValuesResultGlobals = Map.delete hiddenIteratorName globalsNow,
                        toIterableValuesResultLocals = Map.delete hiddenIteratorName localsNow,
                        toIterableValuesResultFunctions = functionsNow,
                        toIterableValuesResultOutputs = outputsNow
                      }
              | otherwise -> Left err
            Right nextResult ->
              collect
                (callBoundMethodResultValue nextResult : acc)
                (callBoundMethodResultGlobals nextResult)
                (callBoundMethodResultLocals nextResult)
                (callBoundMethodResultFunctions nextResult)
                (callBoundMethodResultOutputs nextResult)

    iterMethodConfig currentGlobals currentLocals currentFunctions currentOutputs instanceValue className =
      CallBoundMethodConfig
        { callBoundMethodExecute = toIterableValuesExecute config,
          callBoundMethodIsTopLevel = toIterableValuesIsTopLevel config,
          callBoundMethodPos = pos,
          callBoundMethodInstance = instanceValue,
          callBoundMethodName = "__iter__",
          callBoundMethodMissingMessage = "Type error: " ++ className ++ " is not iterable at " ++ showPos pos,
          callBoundMethodGlobals = currentGlobals,
          callBoundMethodLocals = currentLocals,
          callBoundMethodFunctions = currentFunctions,
          callBoundMethodOutputs = currentOutputs
        }

    nextMethodConfig currentGlobals currentLocals currentFunctions currentOutputs iteratorValue =
      CallBoundMethodConfig
        { callBoundMethodExecute = toIterableValuesExecute config,
          callBoundMethodIsTopLevel = toIterableValuesIsTopLevel config,
          callBoundMethodPos = pos,
          callBoundMethodInstance = iteratorValue,
          callBoundMethodName = "__next__",
          callBoundMethodMissingMessage = "Type error: " ++ classNameOf iteratorValue ++ " is not an iterator at " ++ showPos pos,
          callBoundMethodGlobals = currentGlobals,
          callBoundMethodLocals = currentLocals,
          callBoundMethodFunctions = currentFunctions,
          callBoundMethodOutputs = currentOutputs
        }

    isStopIteration message = "StopIteration" `isPrefixOf` message

    classNameOf iterableValue =
      case iterableValue of
        InstanceValue {instanceValueClass = className} -> className
        _ -> "object"
