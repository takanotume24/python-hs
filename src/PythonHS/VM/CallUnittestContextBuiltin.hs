module PythonHS.VM.CallUnittestContextBuiltin (callUnittestContextBuiltin) where

import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Evaluator.ValueToReplOutput (valueToReplOutput)
import PythonHS.VM.CallUnittestBuiltinConfig (CallUnittestBuiltinConfig (..))

-- | Context-manager and lifecycle helpers used by unittest test cases.
callUnittestContextBuiltin :: CallUnittestBuiltinConfig -> Maybe (Either String Value)
callUnittestContextBuiltin config =
  case name of
    "assertRaises" -> Just (makeAssertRaises args)
    "assertRaisesRegex" -> Just (makeAssertRaisesRegex args)
    "subTest" -> Just (Right (InstanceValue {instanceValueClass = "__SubTestContext__", instanceValueAttrs = []}))
    "__enter__" -> Just (contextEnter args)
    "__exit__" -> Just (contextExit args)
    "setUp" -> Just (Right NoneValue)
    "tearDown" -> Just (Right NoneValue)
    _ -> Nothing
  where
    name = callUnittestBuiltinName config
    args = callUnittestBuiltinArgs config
    pos = callUnittestBuiltinPos config

    makeAssertRaises symbols =
      case symbols of
        [_, expected] ->
          Right
            InstanceValue
              { instanceValueClass = "__AssertRaisesContext__",
                instanceValueAttrs = [("__expected_exception__", StringValue {stringValue = exceptionNameOf expected})]
              }
        _ -> Left ("Argument error: assertRaises supports only the context manager form at " ++ showPos pos)

    makeAssertRaisesRegex symbols =
      case symbols of
        [_, expected, _regex] -> makeAssertRaises [NoneValue, expected]
        _ -> Left ("Argument error: assertRaisesRegex supports only the context manager form at " ++ showPos pos)

    exceptionNameOf value =
      case value of
        ClassValue {classValueName = className} -> className
        ExceptionValue {exceptionValueClass = className} -> className
        StringValue {stringValue = className} -> className
        _ -> valueToReplOutput value

    contextEnter symbols =
      case symbols of
        [InstanceValue {}] -> Right NoneValue
        _ -> Left (countMismatch "__enter__")

    contextExit symbols =
      case symbols of
        [InstanceValue {instanceValueClass = "__SubTestContext__"}, _, _, _] ->
          Right (IntValue {intValue = 0})
        [InstanceValue {instanceValueAttrs = attrs}, raisedClass, _, _] ->
          case raisedClass of
            NoneValue -> Left (assertionFailure ("expected " ++ expectedName attrs ++ " to be raised"))
            StringValue {stringValue = actualName}
              | exceptionNamesMatch (expectedName attrs) actualName -> Right (IntValue {intValue = 1})
              | otherwise -> Right (IntValue {intValue = 0})
            _ -> Right (IntValue {intValue = 0})
        _ -> Left (countMismatch "__exit__")

    exceptionNamesMatch expected actual =
      actual == expected
        || (expected == "TypeError" && actual == "ArgumentError")
        || (expected == "ValueError" && actual == "ArgumentError")

    expectedName attrs =
      case lookup "__expected_exception__" attrs of
        Just (StringValue {stringValue = className}) -> className
        _ -> "Exception"

    assertionFailure message = "AssertionError: " ++ message ++ " at " ++ showPos pos

    countMismatch functionName =
      "Argument count mismatch when calling " ++ functionName ++ " at " ++ showPos pos
