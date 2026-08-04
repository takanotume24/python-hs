module PythonHS.Evaluator.LookupDictValueConfig (LookupDictValueConfig (..)) where

import PythonHS.Evaluator.DictPair (DictPair)
import PythonHS.Evaluator.Value (Value)

data LookupDictValueConfig = LookupDictValueConfig
  { lookupDictValueConfigPairs :: [DictPair Value Value],
    lookupDictValueConfigKey :: Value
  }
