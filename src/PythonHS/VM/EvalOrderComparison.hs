module PythonHS.VM.EvalOrderComparison (evalOrderComparison) where

import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value (..))
import PythonHS.VM.EvalOrderComparisonConfig (EvalOrderComparisonConfig (..))

-- | Evaluate @<@, @>@, @<=@ and @>=@ for numbers, dataclass instances and
-- tuples, falling back to numeric comparison for every other value.
evalOrderComparison :: EvalOrderComparisonConfig -> Either String Value
evalOrderComparison config =
  let context = evalOrderComparisonContext config
      pos = evalOrderComparisonPos config
      left = evalOrderComparisonLeft config
      right = evalOrderComparisonRight config
      cmp = evalOrderComparisonCompareResult config
      fallback = numericComparison (\leftNumber rightNumber -> cmp (compare leftNumber rightNumber))
   in case left of
        InstanceValue {instanceValueClass = leftClass, instanceValueAttrs = leftAttrs} ->
          case right of
            InstanceValue {instanceValueClass = rightClass, instanceValueAttrs = rightAttrs} ->
              if leftClass == rightClass
                then do
                  ordResult <- compareLists (filterDataclassAttrs leftAttrs) (filterDataclassAttrs rightAttrs)
                  Right (IntValue {intValue = if cmp ordResult then 1 else 0})
                else Left ("Type error: expected int in " ++ context ++ " at " ++ showPos pos)
            _ -> fallback
        TupleValue {tupleValueItems = leftValues} ->
          case right of
            TupleValue {tupleValueItems = rightValues} -> do
              ordResult <- compareLists leftValues rightValues
              Right (IntValue {intValue = if cmp ordResult then 1 else 0})
            _ -> fallback
        _ -> fallback
  where
    numericComparison compareNumbers = do
      leftNumber <- expectNumber (evalOrderComparisonContext config) (evalOrderComparisonPos config) (evalOrderComparisonLeft config)
      rightNumber <- expectNumber (evalOrderComparisonContext config) (evalOrderComparisonPos config) (evalOrderComparisonRight config)
      Right (IntValue {intValue = if compareNumbers leftNumber rightNumber then 1 else 0})

    filterDataclassAttrs [] = []
    filterDataclassAttrs (pair : rest) =
      if fst pair == "__python_hs_frozen__"
        then filterDataclassAttrs rest
        else snd pair : filterDataclassAttrs rest

    compareLists [] [] = Right EQ
    compareLists [] (_ : _) = Right LT
    compareLists (_ : _) [] = Right GT
    compareLists (leftValue : leftRest) (rightValue : rightRest) =
      let firstCompare = compareSingleValue leftValue rightValue
       in if firstCompare == EQ
            then compareLists leftRest rightRest
            else Right firstCompare

    compareSingleValue (IntValue {intValue = leftInt}) (IntValue {intValue = rightInt}) = compare leftInt rightInt
    compareSingleValue (FloatValue {floatValue = leftFloat}) (FloatValue {floatValue = rightFloat}) = compare leftFloat rightFloat
    compareSingleValue (IntValue {intValue = leftInt}) (FloatValue {floatValue = rightFloat}) = compare (fromIntegral leftInt :: Double) rightFloat
    compareSingleValue (FloatValue {floatValue = leftFloat}) (IntValue {intValue = rightInt}) = compare leftFloat (fromIntegral rightInt :: Double)
    compareSingleValue (StringValue {stringValue = leftString}) (StringValue {stringValue = rightString}) = compare leftString rightString
    compareSingleValue leftValue rightValue = compare (show leftValue) (show rightValue)

    expectNumber _ _ IntValue {intValue = n} = Right (fromIntegral n)
    expectNumber _ _ FloatValue {floatValue = n} = Right n
    expectNumber _ _ NoneValue = Right 0
    expectNumber errorContext errorPos _ = Left ("Type error: expected int in " ++ errorContext ++ " at " ++ showPos errorPos)
