module PythonHS.Evaluator.RemoveFirstValue (removeFirstValue) where

import PythonHS.Evaluator.RemoveFirstValueConfig (RemoveFirstValueConfig (..))
import PythonHS.Evaluator.Value (Value)

removeFirstValue :: RemoveFirstValueConfig -> Maybe [Value]
removeFirstValue config = go (removeFirstValueConfigValues config) (removeFirstValueConfigTarget config)
  where
    go [] _ = Nothing
    go (v : restVals) target
      | v == target = Just restVals
      | otherwise = fmap (v :) (go restVals target)
