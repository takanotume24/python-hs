module PythonHS.VM.NumberPairs (numberPairs) where

import PythonHS.Evaluator.NumberSortPair (NumberSortPair (..))
import PythonHS.Evaluator.Value (Value (..))

numberPairs :: [Value] -> Maybe [NumberSortPair]
numberPairs [] = Just []
numberPairs (value : restValues) =
  case value of
    IntValue {intValue = n} ->
      fmap
        ( NumberSortPair {numberSortPairSortKey = fromIntegral n, numberSortPairValue = IntValue {intValue = n}} :
        )
        (numberPairs restValues)
    FloatValue {floatValue = n} ->
      fmap
        ( NumberSortPair {numberSortPairSortKey = n, numberSortPairValue = FloatValue {floatValue = n}} :
        )
        (numberPairs restValues)
    _ -> Nothing
