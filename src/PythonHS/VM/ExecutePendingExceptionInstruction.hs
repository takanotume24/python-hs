module PythonHS.VM.ExecutePendingExceptionInstruction (executePendingExceptionInstruction) where

import Data.Map.Strict qualified as Map
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Evaluator.ValueToOutput (valueToOutput)
import PythonHS.VM.Instruction (Instruction (LoadPendingException, LoadPendingExceptionClass, LoadPendingExceptionMessage, MatchExceptionType, RaisePendingException))

executePendingExceptionInstruction :: String -> Instruction -> [Value] -> Map.Map String Value -> Either String (Maybe String, [Value])
executePendingExceptionInstruction pendingExceptErrorName instruction stack localEnv =
  case instruction of
    LoadPendingException ->
      case Map.lookup pendingExceptValueName localEnv of
        Just value -> Right (Nothing, value : stack)
        Nothing -> Left "VM runtime error: pending exception is not set"
    LoadPendingExceptionClass ->
      case Map.lookup pendingExceptValueName localEnv of
        Just value -> Right (Nothing, StringValue (exceptionClassName value) : stack)
        Nothing -> Left "VM runtime error: pending exception is not set"
    LoadPendingExceptionMessage ->
      case Map.lookup pendingExceptValueName localEnv of
        Just value -> Right (Nothing, StringValue (exceptionMessageText value) : stack)
        Nothing -> Left "VM runtime error: pending exception is not set"
    MatchExceptionType maybeTypeName ->
      let matched =
            case Map.lookup pendingExceptErrorName localEnv of
              Just StringValue {stringValue = err} -> matchesExceptionType maybeTypeName err
              _ -> False
          result = if matched then 1 else 0
       in Right (Nothing, IntValue result : stack)
    RaisePendingException ->
      case Map.lookup pendingExceptErrorName localEnv of
        Just StringValue {stringValue = err} -> Right (Just err, stack)
        _ -> Left "Runtime error: No active exception to re-raise"
    _ -> Left "VM runtime error: executePendingExceptionInstruction called with non-pending-exception instruction"
  where
    pendingExceptValueName = "__python_hs_pending_except_value__"

    matchesExceptionType maybeTypeNames err =
      case maybeTypeNames of
        Nothing -> True
        Just typeNames -> any (matchesTypeName err) typeNames

    matchesTypeName err typeName =
      case typeName of
        "Exception" -> True
        "BaseException" -> True
        _ ->
          case normalizeErrorType typeName of
            Just expectedPrefix ->
              startsWith expectedPrefix err
                || isSubclassOf (errorClassName err) typeName localEnv
            Nothing ->
              startsWith (typeName ++ ": ") err
                || isSubclassOf (errorClassName err) typeName localEnv

    normalizeErrorType typeName
      | typeName == "RuntimeError" = Just "Runtime error:"
      | typeName == "TypeError" = Just "Type error:"
      | typeName == "ValueError" = Just "Value error:"
      | typeName == "NameError" = Just "Name error:"
      | typeName == "ArgumentError" = Just "Argument error:"
      | typeName == "ImportError" = Just "Import error:"
      | typeName == "UnboundLocalError" = Just "UnboundLocalError:"
      | typeName == "AttributeError" = Just "Attribute error:"
      | typeName == "AssertionError" = Just "AssertionError:"
      | otherwise = Nothing

    startsWith prefix value = take (length prefix) value == prefix

    exceptionClassName value =
      case value of
        ExceptionValue {exceptionValueClass = className} -> className
        InstanceValue {instanceValueClass = className} -> className
        _ -> "RuntimeError"

    exceptionMessageText value =
      case value of
        ExceptionValue {exceptionValueMessage = message} -> message
        InstanceValue {instanceValueAttrs = attrs} ->
          case lookup "message" attrs of
            Just (StringValue {stringValue = message}) -> message
            _ -> ""
        _ -> valueToOutput value

    errorClassName err =
      case break (== ':') err of
        (className, ':' : _) -> className
        _ -> ""

    isSubclassOf className typeName env =
      className /= "" && className /= typeName && anyBaseIs typeName env className

    anyBaseIs target env className =
      case Map.lookup className env of
        Just ClassValue {classValueBase = Just baseName} ->
          baseName == target || anyBaseIs target env baseName
        _ -> False
