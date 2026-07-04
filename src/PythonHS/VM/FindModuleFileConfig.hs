module PythonHS.VM.FindModuleFileConfig (FindModuleFileConfig (..)) where

data FindModuleFileConfig = FindModuleFileConfig
  { findModuleFileModulePath :: [String],
    findModuleFilePaths :: [FilePath]
  }
