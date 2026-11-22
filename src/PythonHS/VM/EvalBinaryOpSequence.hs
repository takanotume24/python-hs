module PythonHS.VM.EvalBinaryOpSequence (evalBinaryOpSequence) where

import PythonHS.AST.BinaryOperator (BinaryOperator (..))
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.EvalBinaryOpSequenceConfig (EvalBinaryOpSequenceConfig (..))

-- | Handle @+@ and @*@ when either operand is a string, list, or tuple.
--
-- Returns Nothing for every other combination so the numeric operator
-- implementations can handle the expression.
evalBinaryOpSequence :: EvalBinaryOpSequenceConfig -> Maybe (Either String Value)
evalBinaryOpSequence config =
  case evalBinaryOpSequenceOp config of
    AddOperator -> concatSequences
    MultiplyOperator -> repeatSequence
    _ -> Nothing
  where
    left = evalBinaryOpSequenceLeft config
    right = evalBinaryOpSequenceRight config
    pos = evalBinaryOpSequencePos config

    concatSequences =
      case (left, right) of
        (StringValue {stringValue = leftText}, StringValue {stringValue = rightText}) ->
          Just (Right (StringValue {stringValue = leftText ++ rightText}))
        (ListValue {listValueItems = leftItems}, ListValue {listValueItems = rightItems}) ->
          Just (Right (ListValue {listValueItems = leftItems ++ rightItems}))
        (TupleValue {tupleValueItems = leftItems}, TupleValue {tupleValueItems = rightItems}) ->
          Just (Right (TupleValue {tupleValueItems = leftItems ++ rightItems}))
        (BytesValue {bytesValueItems = leftItems, bytesValueIsMutable = isMutable}, BytesValue {bytesValueItems = rightItems}) ->
          Just (Right (BytesValue {bytesValueItems = leftItems ++ rightItems, bytesValueIsMutable = isMutable}))
        _ -> Nothing

    repeatSequence =
      case (left, right) of
        (StringValue {stringValue = text}, IntValue {intValue = count}) ->
          Just (Right (StringValue {stringValue = repeatText text count}))
        (IntValue {intValue = count}, StringValue {stringValue = text}) ->
          Just (Right (StringValue {stringValue = repeatText text count}))
        (ListValue {listValueItems = items}, IntValue {intValue = count}) ->
          Just (Right (ListValue {listValueItems = repeatItems items count}))
        (IntValue {intValue = count}, ListValue {listValueItems = items}) ->
          Just (Right (ListValue {listValueItems = repeatItems items count}))
        (TupleValue {tupleValueItems = items}, IntValue {intValue = count}) ->
          Just (Right (TupleValue {tupleValueItems = repeatItems items count}))
        (IntValue {intValue = count}, TupleValue {tupleValueItems = items}) ->
          Just (Right (TupleValue {tupleValueItems = repeatItems items count}))
        (BytesValue {bytesValueItems = items, bytesValueIsMutable = isMutable}, IntValue {intValue = count}) ->
          Just (Right (BytesValue {bytesValueItems = repeatItems items count, bytesValueIsMutable = isMutable}))
        (IntValue {intValue = count}, BytesValue {bytesValueItems = items, bytesValueIsMutable = isMutable}) ->
          Just (Right (BytesValue {bytesValueItems = repeatItems items count, bytesValueIsMutable = isMutable}))
        (StringValue {}, _) -> Just (Left typeError)
        (ListValue {}, _) -> Just (Left typeError)
        (TupleValue {}, _) -> Just (Left typeError)
        (BytesValue {}, _) -> Just (Left typeError)
        _ -> Nothing

    repeatText text count = concat (replicate (repeatCount count) text)

    repeatItems :: [a] -> Integer -> [a]
    repeatItems items count = concat (replicate (repeatCount count) items)

    repeatCount count
      | count <= 0 = 0
      | count > fromIntegral maxRepeat = maxRepeat
      | otherwise = fromIntegral count

    maxRepeat :: Int
    maxRepeat = 1000000

    typeError = "Type error: cannot multiply sequence at " ++ showPos pos
