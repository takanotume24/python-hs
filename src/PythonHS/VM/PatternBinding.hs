module PythonHS.VM.PatternBinding (PatternBinding (..)) where

import PythonHS.Evaluator.Value (Value)

data PatternBinding = PatternBinding
  { patternBindingName :: String,
    patternBindingValue :: Value
  }
  deriving (Eq, Show)
