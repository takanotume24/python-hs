module PythonHS.VM.ExitKind (ExitKind (..)) where

data ExitKind = ExitBreak | ExitContinue
  deriving (Eq)
