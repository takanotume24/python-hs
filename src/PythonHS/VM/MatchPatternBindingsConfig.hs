module PythonHS.VM.MatchPatternBindingsConfig (MatchPatternBindingsConfig (..)) where

import PythonHS.AST.Pattern (Pattern)
import PythonHS.Evaluator.Value (Value)

data MatchPatternBindingsConfig = MatchPatternBindingsConfig
  { matchPatternBindingsPattern :: Pattern,
    matchPatternBindingsSubject :: Value
  }
