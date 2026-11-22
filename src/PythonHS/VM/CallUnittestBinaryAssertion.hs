module PythonHS.VM.CallUnittestBinaryAssertion (callUnittestBinaryAssertion) where

import Data.List (isInfixOf)
import PythonHS.Evaluator.DictPair (DictPair (..))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.Evaluator.ValueToReplOutput (valueToReplOutput)
import PythonHS.Evaluator.ValuesEqual (valuesEqual)
import PythonHS.VM.CallUnittestBuiltinConfig (CallUnittestBuiltinConfig (..))
import PythonHS.VM.IsTruthy (isTruthy)

-- | Binary and value-based assertion helpers used by unittest test cases.
callUnittestBinaryAssertion :: CallUnittestBuiltinConfig -> Maybe (Either String Value)
callUnittestBinaryAssertion config =
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
    "assertIsInstance" -> Just (assertInstance True args)
    "assertNotIsInstance" -> Just (assertInstance False args)
    "assertGreater" -> Just (assertCompare (\ordering -> ordering == GT) ">" args)
    "assertGreaterEqual" -> Just (assertCompare (\ordering -> ordering /= LT) ">=" args)
    "assertLess" -> Just (assertCompare (\ordering -> ordering == LT) "<" args)
    "assertLessEqual" -> Just (assertCompare (\ordering -> ordering /= GT) "<=" args)
    "assertAlmostEqual" -> Just (assertAlmostEqual args)
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

    assertInstance expected symbols =
      case symbols of
        [_, value, expectedClass]
          | (typeNameOf value == classNameOf expectedClass) == expected -> Right NoneValue
          | otherwise ->
              Left
                ( assertionFailure
                    (valueToReplOutput value ++ " is " ++ typeNameOf value ++ ", expected " ++ classNameOf expectedClass)
                )
        _ -> Left (countMismatch name)

    assertCompare predicate operator symbols =
      case symbols of
        [_, leftValue, rightValue] ->
          case compareValue leftValue rightValue of
            Just ordering
              | predicate ordering -> Right NoneValue
            _ ->
              Left
                ( assertionFailure
                    (valueToReplOutput leftValue ++ " " ++ operator ++ " " ++ valueToReplOutput rightValue ++ " is false")
                )
        _ -> Left (countMismatch name)

    compareValue leftValue rightValue =
      case (leftValue, rightValue) of
        (IntValue {intValue = leftInt}, IntValue {intValue = rightInt}) -> Just (compare leftInt rightInt)
        (FloatValue {floatValue = leftFloat}, FloatValue {floatValue = rightFloat}) -> Just (compare leftFloat rightFloat)
        (IntValue {intValue = leftInt}, FloatValue {floatValue = rightFloat}) -> Just (compare (fromIntegral leftInt :: Double) rightFloat)
        (FloatValue {floatValue = leftFloat}, IntValue {intValue = rightInt}) -> Just (compare leftFloat (fromIntegral rightInt :: Double))
        (StringValue {stringValue = leftString}, StringValue {stringValue = rightString}) -> Just (compare leftString rightString)
        _ -> Nothing

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
        FunctionRefValue {functionRefValueName = functionName} -> functionName
        ClassValue {} -> "type"
        InstanceValue {instanceValueClass = className} -> className
        ExceptionValue {exceptionValueClass = className} -> className
        _ -> "object"

    classNameOf value =
      case value of
        ClassValue {classValueName = className} -> className
        FunctionRefValue {functionRefValueName = functionName} -> functionName
        ExceptionValue {exceptionValueClass = className} -> className
        StringValue {stringValue = className} -> className
        other -> valueToReplOutput other

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
