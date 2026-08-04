module PythonHS.VM.MethodNamePair (MethodNamePair (..)) where

data MethodNamePair = MethodNamePair
  { methodNamePairOriginal :: String,
    methodNamePairMangled :: String
  }
  deriving (Eq, Show)
