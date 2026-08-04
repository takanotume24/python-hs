module PythonHS.Evaluator.PairToList (pairToList) where

import PythonHS.Evaluator.DictPair (DictPair (..))
import PythonHS.Evaluator.Value (Value (ListValue), listValueItems)

pairToList :: DictPair Value Value -> Value
pairToList pair = ListValue {listValueItems = [dictPairKey pair, dictPairValue pair]}
