module PythonHS.Evaluator.LookupDictValue (lookupDictValue) where

import PythonHS.Evaluator.DictPair (DictPair (..))
import PythonHS.Evaluator.LookupDictValueConfig (LookupDictValueConfig (..))
import PythonHS.Evaluator.Value (Value)

lookupDictValue :: LookupDictValueConfig -> Maybe Value
lookupDictValue config = go (lookupDictValueConfigPairs config) (lookupDictValueConfigKey config)
  where
    go [] _ = Nothing
    go (entry : restPairs) target
      | dictPairKey entry == target = Just (dictPairValue entry)
      | otherwise = go restPairs target
