module PythonHS.VM.IsStdlibFromImportModule (isStdlibFromImportModule) where

-- | Modules whose @from ... import ...@ forms are handled by the compiler
-- instead of by loading a Python source file from the search path.
isStdlibFromImportModule :: [String] -> Bool
isStdlibFromImportModule modulePath =
  modulePath `elem` builtinModules
  where
    builtinModules =
      [ ["math"],
        ["dataclasses"],
        ["os"],
        ["json"],
        ["pathlib"],
        ["sys"],
        ["time"],
        ["random"]
      ]
