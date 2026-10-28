module PythonHS.VM.MethodNamePair (MethodNamePair (..)) where

import PythonHS.VM.MethodKind (MethodKind)

data MethodNamePair = MethodNamePair
  { methodNamePairOriginal :: String,
    methodNamePairMangled :: String,
    methodNamePairKind :: MethodKind
  }
  deriving (Eq, Show)
