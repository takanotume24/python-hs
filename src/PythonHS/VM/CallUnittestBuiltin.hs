module PythonHS.VM.CallUnittestBuiltin (callUnittestBuiltin) where

import Data.List (isInfixOf)
import PythonHS.Evaluator.DictPair (DictPair (..))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Evaluator.ValueToReplOutput (valueToReplOutput)
import PythonHS.Evaluator.ValuesEqual (valuesEqual)
import PythonHS.VM.CallUnittestBuiltinConfig (CallUnittestBuiltinConfig (..))
import PythonHS.VM.IsTruthy (isTruthy)

-- | Minimal assertion helpers used by CPython's unittest test cases.
callUnittestBuiltin :: CallUnittestBuiltinConfig -> Maybe (Either String Value)
callUnittestBuiltin config =
  case name of
    "assertEqual" -> Just (assertBinary (\left right -> valuesEqual left right) "!=" args)
    "assertNotEqual" -> Just (assertBinary (\left right -> not (valuesEqual left right)) "==" args)
    "assertListEqual" -> Just (assertBinary (\left right -> valuesEqual left right) "!=" args)
    "assertTupleEqual" -> Just (assertBinary (\left right -> valuesEqual left right) "!=" args)
    "assertDictEqual" -> Just (assertBinary (\left right -> valuesEqual left right) "!=" args)
    "assertTrue" -> Just (assertTruthy True args)
    "assertFalse" -> Just (assertTruthy False args)
    "assertIs" -> Just (assertBinary (\left right -> left == right) "is not" args)
    "assertIsNot" -> Just (assertBinary (\left right -> left /= right) "is" args)
    "assertIsNone" -> Just (assertNone True args)
    "assertIsNotNone" -> Just (assertNone False args)
    "assertIn" -> Just (assertMembership True args)
    "assertNotIn" -> Just (assertMembership False args)
    "assertAlmostEqual" -> Just (assertAlmostEqual args)
    "assertRaises" -> Just (makeAssertRaises args)
    "__enter__" -> Just (contextEnter args)
    "__exit__" -> Just (contextExit args)
    "setUp" -> Just (Right NoneValue)
    "tearDown" -> Just (Right NoneValue)
    "fail" -> Just (assertFail args)
    _ -> Nothing
  where
    name = callUnittestBuiltinName config
    args = callUnittestBuiltinArgs config
    pos = callUnittestBuiltinPos config

    assertBinary predicate operator symbols =
      case symbols of
        [_, left, right]
          | predicate left right -> Right NoneValue
          | otherwise ->
              Left
                ( assertionFailure
                    (valueToReplOutput left ++ " " ++ operator ++ " " ++ valueToReplOutput right)
                )
        _ -> Left (countMismatch name)

    assertTruthy expected symbols =
      case symbols of
        [_, value]
          | isTruthy value == expected -> Right NoneValue
          | expected -> Left (assertionFailure "expected True")
          | otherwise -> Left (assertionFailure "expected False")
        _ -> Left (countMismatch name)

    assertNone expected symbols =
      case symbols of
        [_, value]
          | isNone value == expected -> Right NoneValue
          | expected -> Left (assertionFailure (valueToReplOutput value ++ " is not None"))
          | otherwise -> Left (assertionFailure "value is None")
        _ -> Left (countMismatch name)

    assertMembership expected symbols =
      case symbols of
        [_, needle, container]
          | contains needle container == expected -> Right NoneValue
          | expected -> Left (assertionFailure (valueToReplOutput needle ++ " not found in " ++ valueToReplOutput container))
          | otherwise -> Left (assertionFailure (valueToReplOutput needle ++ " unexpectedly found in " ++ valueToReplOutput container))
        _ -> Left (countMismatch name)

    assertFail symbols =
      case symbols of
        [_] -> Left (assertionFailure "test failed")
        [_, message] -> Left (assertionFailure (valueToReplOutput message))
        _ -> Left (countMismatch name)

    assertAlmostEqual symbols =
      case symbols of
        [_, leftValue, rightValue] -> compareAlmost 7 leftValue rightValue
        [_, leftValue, rightValue, IntValue {intValue = places}] -> compareAlmost (fromIntegral places) leftValue rightValue
        _ -> Left (countMismatch name)

    compareAlmost :: Integer -> Value -> Value -> Either String Value
    compareAlmost places leftValue rightValue =
      case (asDouble leftValue, asDouble rightValue) of
        (Just leftNumber, Just rightNumber) ->
          if abs (leftNumber - rightNumber) <= tolerance
            then Right NoneValue
            else Left (assertionFailure (valueToReplOutput leftValue ++ " != " ++ valueToReplOutput rightValue))
        _ ->
          if valuesEqual leftValue rightValue
            then Right NoneValue
            else Left (assertionFailure (valueToReplOutput leftValue ++ " != " ++ valueToReplOutput rightValue))
      where
        tolerance = 0.5 * (10 ** negate (fromIntegral places :: Double))

    asDouble value =
      case value of
        IntValue {intValue = number} -> Just (fromIntegral number)
        FloatValue {floatValue = number} -> Just number
        _ -> Nothing

    makeAssertRaises symbols =
      case symbols of
        [_, expected] ->
          Right
            InstanceValue
              { instanceValueClass = "__AssertRaisesContext__",
                instanceValueAttrs = [("__expected_exception__", StringValue {stringValue = exceptionNameOf expected})]
              }
        _ -> Left ("Argument error: assertRaises supports only the context manager form at " ++ showPos pos)

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
        [InstanceValue {instanceValueAttrs = attrs}, raisedClass, _, _] ->
          case raisedClass of
            NoneValue -> Left (assertionFailure ("expected " ++ expectedName attrs ++ " to be raised"))
            StringValue {stringValue = actualName}
              | actualName == expectedName attrs -> Right (IntValue {intValue = 1})
              | otherwise -> Right (IntValue {intValue = 0})
            _ -> Right (IntValue {intValue = 0})
        _ -> Left (countMismatch "__exit__")

    expectedName attrs =
      case lookup "__expected_exception__" attrs of
        Just (StringValue {stringValue = className}) -> className
        _ -> "Exception"

    isNone value =
      case value of
        NoneValue -> True
        _ -> False

    contains needle container =
      case container of
        ListValue {listValueItems = items} -> any (valuesEqual needle) items
        TupleValue {tupleValueItems = items} -> any (valuesEqual needle) items
        DictValue {dictValuePairs = pairs} -> any (valuesEqual needle . dictPairKey) pairs
        StringValue {stringValue = haystack} ->
          case needle of
            StringValue {stringValue = text} -> text `isInfixOf` haystack
            _ -> False
        _ -> False

    dictPairKey (DictPair key _) = key

    assertionFailure message = "AssertionError: " ++ message ++ " at " ++ showPos pos

    countMismatch functionName =
      "Argument count mismatch when calling " ++ functionName ++ " at " ++ showPos pos
