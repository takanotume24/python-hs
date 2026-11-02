module PythonHS.VM.ExecutePendingExceptionInstruction (executePendingExceptionInstruction) where

import Data.Map.Strict qualified as Map
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.Instruction (Instruction (LoadPendingException, MatchExceptionType, RaisePendingException))

executePendingExceptionInstruction :: String -> Instruction -> [Value] -> Map.Map String Value -> Either String (Maybe String, [Value])
executePendingExceptionInstruction pendingExceptErrorName instruction stack localEnv =
  case instruction of
    LoadPendingException ->
      case Map.lookup pendingExceptErrorName localEnv of
        Just value -> Right (Nothing, value : stack)
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
        _ -> Left "VM runtime error: pending exception is not set"
    _ -> Left "VM runtime error: executePendingExceptionInstruction called with non-pending-exception instruction"
  where
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
            Nothing -> False
            Just expectedPrefix -> startsWith expectedPrefix err

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
