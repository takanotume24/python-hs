module PythonHS.VM.IsBuiltinImportModule (isBuiltinImportModule) where

isBuiltinImportModule :: [String] -> Bool
isBuiltinImportModule modulePath =
  modulePath == ["math"]
    || modulePath == ["dataclasses"]
    || modulePath == ["os"]
    || modulePath == ["json"]
    || modulePath == ["pathlib"]
