module PythonHS.VM.DataclassFlags (DataclassFlags (..)) where

data DataclassFlags = DataclassFlags
  { dataclassFlagsFrozen :: Bool,
    dataclassFlagsOrder :: Bool
  }
  deriving (Eq, Show)
