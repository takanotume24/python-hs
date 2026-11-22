module PythonHS.Evaluator.ValuesEqual (valuesEqual) where

import PythonHS.Evaluator.Value (Value (..))

-- | Compare two values with Python-like numeric equality.
valuesEqual :: Value -> Value -> Bool
valuesEqual (IntValue {intValue = leftInt}) (IntValue {intValue = rightInt}) = leftInt == rightInt
valuesEqual (IntValue {intValue = leftInt}) (FloatValue {floatValue = rightFloat}) = (fromIntegral leftInt :: Double) == rightFloat
valuesEqual (FloatValue {floatValue = leftFloat}) (IntValue {intValue = rightInt}) = leftFloat == (fromIntegral rightInt :: Double)
valuesEqual (FloatValue {floatValue = leftFloat}) (FloatValue {floatValue = rightFloat}) = leftFloat == rightFloat
valuesEqual (StringValue {stringValue = leftString}) (StringValue {stringValue = rightString}) = leftString == rightString
valuesEqual (ListValue {listValueItems = leftItems}) (ListValue {listValueItems = rightItems}) = leftItems == rightItems
valuesEqual (TupleValue {tupleValueItems = leftItems}) (TupleValue {tupleValueItems = rightItems}) = leftItems == rightItems
valuesEqual (SetValue {setValueItems = leftItems}) (SetValue {setValueItems = rightItems}) =
  length leftItems == length rightItems
    && all (\leftItem -> any (valuesEqual leftItem) rightItems) leftItems
valuesEqual (BytesValue {bytesValueItems = leftItems}) (BytesValue {bytesValueItems = rightItems}) = leftItems == rightItems
valuesEqual leftValue rightValue = leftValue == rightValue
