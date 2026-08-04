module PythonHS.VM.MappingMatchResult (MappingMatchResult (..)) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.VM.PatternBinding (PatternBinding)

data MappingMatchResult = MappingMatchResult
  { mappingMatchResultBindings :: [PatternBinding],
    mappingMatchResultMatchedKeys :: [Value]
  }
  deriving (Eq, Show)
