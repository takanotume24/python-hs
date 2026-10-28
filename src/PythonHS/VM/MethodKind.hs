module PythonHS.VM.MethodKind (MethodKind (..)) where

data MethodKind
  = InstanceMethod
  | StaticMethod
  | ClassMethod
  deriving (Eq, Show)
