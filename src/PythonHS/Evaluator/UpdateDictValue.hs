module PythonHS.Evaluator.UpdateDictValue (updateDictValue) where

import PythonHS.Evaluator.DictPair (DictPair (..))
import PythonHS.Evaluator.UpdateDictValueConfig (UpdateDictValueConfig (..))
import PythonHS.Evaluator.Value (Value)

updateDictValue :: UpdateDictValueConfig -> [DictPair Value Value]
updateDictValue config = go (updateDictValueConfigPairs config) (updateDictValueConfigKey config) (updateDictValueConfigValue config)
  where
    go [] key value = [DictPair {dictPairKey = key, dictPairValue = value}]
    go (entry : restPairs) key value
      | dictPairKey entry == key = DictPair {dictPairKey = key, dictPairValue = value} : restPairs
      | otherwise = entry : go restPairs key value
