module PythonHS.Evaluator.UpdateDictValueConfig (UpdateDictValueConfig (..)) where

import PythonHS.Evaluator.DictPair (DictPair)
import PythonHS.Evaluator.Value (Value)

data UpdateDictValueConfig = UpdateDictValueConfig
  { updateDictValueConfigPairs :: [DictPair Value Value],
    updateDictValueConfigKey :: Value,
    updateDictValueConfigValue :: Value
  }
