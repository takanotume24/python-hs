module PythonHS.VM.ToPairs (toPairs) where

import PythonHS.Evaluator.DictPair (DictPair (..))
import PythonHS.Evaluator.Value (Value)

toPairs :: [Value] -> Either String [DictPair Value Value]
toPairs values =
  case values of
    [] -> Right []
    key : value : rest -> do
      remaining <- toPairs rest
      Right (DictPair {dictPairKey = key, dictPairValue = value} : remaining)
    _ -> Left "VM runtime error: dictionary build expects key/value pairs"
