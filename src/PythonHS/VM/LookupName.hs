module PythonHS.VM.LookupName (lookupName) where

import qualified Data.Map.Strict as Map
import PythonHS.Evaluator.Value (Value)

lookupName :: String -> Map.Map String Value -> Map.Map String Value -> Maybe Value
lookupName name localEnv globalsEnv =
  case Map.lookup name localEnv of
    Just value -> Just value
    Nothing -> Map.lookup name globalsEnv
