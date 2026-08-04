module PythonHS.Evaluator.DictPair (DictPair (..)) where

data DictPair k v = DictPair
  { dictPairKey :: k,
    dictPairValue :: v
  }
  deriving (Eq, Show)
