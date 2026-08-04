module PythonHS.Evaluator.NumberPairs (numberPairs) where

import PythonHS.Evaluator.NumberPairsConfig (NumberPairsConfig (..))
import PythonHS.Evaluator.NumberSortPair (NumberSortPair (..))
import PythonHS.Evaluator.Value (Value (..))

numberPairs :: NumberPairsConfig -> Maybe [NumberSortPair]
numberPairs config = go (numberPairsConfigValues config)
  where
    go [] = Just []
    go (IntValue {intValue = n} : restVals) = fmap (NumberSortPair {numberSortPairSortKey = fromIntegral n, numberSortPairValue = IntValue {intValue = n}} :) (go restVals)
    go (FloatValue {floatValue = n} : restVals) = fmap (NumberSortPair {numberSortPairSortKey = n, numberSortPairValue = FloatValue {floatValue = n}} :) (go restVals)
    go (_ : _) = Nothing
