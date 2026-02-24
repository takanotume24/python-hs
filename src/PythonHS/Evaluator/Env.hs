module PythonHS.Evaluator.Env (Env) where

import Data.Map.Strict (Map)
import PythonHS.Evaluator.Value (Value)

type Env = Map String Value
