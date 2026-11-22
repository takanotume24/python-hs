module PythonHS.VM.CallReflectionBuiltin (callReflectionBuiltin) where

import Data.Map.Strict qualified as Map
import PythonHS.Evaluator.ModuleAttr (ModuleAttr (..))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.CallReflectionBuiltinConfig (CallReflectionBuiltinConfig (..))
import PythonHS.VM.FunctionInfo (FunctionInfo)
import PythonHS.VM.LookupName (lookupName)
import PythonHS.VM.MethodNamePair (MethodNamePair (..))
import PythonHS.VM.ModulePrefixFor (modulePrefixFor)
import PythonHS.VM.SplitByDot (splitByDot)

callReflectionBuiltin :: CallReflectionBuiltinConfig -> Maybe (Either String Value)
callReflectionBuiltin config =
  case name of
    "type" -> Just (evalType args)
    "isinstance" -> Just (evalIsInstance args)
    "hasattr" -> Just (evalHasAttr args)
    _ -> Nothing
  where
    name = callReflectionBuiltinName config
    args = callReflectionBuiltinArgs config
    pos = callReflectionBuiltinPos config
    globals = callReflectionBuiltinGlobals config
    locals = callReflectionBuiltinLocals config
    functions = callReflectionBuiltinFunctions config

    evalType values =
      case values of
        [value] -> Right (StringValue {stringValue = typeNameOf value})
        _ -> Left (countMismatch "type")

    evalIsInstance values =
      case values of
        [value, StringValue {stringValue = className}] ->
          Right (IntValue {intValue = if typeNameOf value == className then 1 else 0})
        [_, _] -> Left ("Type error: isinstance expects class name string at " ++ showPos pos)
        _ -> Left (countMismatch "isinstance")

    evalHasAttr values =
      case values of
        [value, StringValue {stringValue = attributeName}] ->
          Right (IntValue {intValue = if hasAttribute value attributeName then 1 else 0})
        [_, _] -> Left ("Type error: hasattr expects attribute name string at " ++ showPos pos)
        _ -> Left (countMismatch "hasattr")

    typeNameOf value =
      case value of
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

    hasAttribute value attributeName =
      case value of
        ModuleValue {} -> moduleHasAttribute value attributeName
        InstanceValue {instanceValueClass = className, instanceValueAttrs = attrs} ->
          any ((== attributeName) . fst) attrs || classHasAttribute className attributeName
        ClassValue {classValueName = className} -> classHasAttribute className attributeName
        _ -> False

    moduleHasAttribute moduleValue attributeName =
      any ((== attributeName) . moduleAttrName) (moduleValueAttributes moduleValue)
        || Map.member (modulePrefixFor (splitByDot (moduleValueNameOf moduleValue)) ++ attributeName) functions

    classHasAttribute className attributeName =
      case lookupName className locals globals of
        Just (ClassValue {classValueBase = maybeBase, classValueMethods = methods, classValueAttrs = attrs}) ->
          any ((== attributeName) . fst) attrs
            || any ((== attributeName) . methodNamePairOriginal) methods
            || maybe False (`classHasAttribute` attributeName) maybeBase
        _ -> False

    moduleValueAttributes value =
      case value of
        ModuleValue {moduleValueAttrs = attrs} -> attrs
        _ -> []

    moduleValueNameOf value =
      case value of
        ModuleValue {moduleValueName = moduleName} -> moduleName
        _ -> ""

    countMismatch fname = "Argument count mismatch when calling " ++ fname ++ " at " ++ showPos pos
