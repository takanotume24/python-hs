module PythonHS.VM.ResolveTargetModulePathConfig (ResolveTargetModulePathConfig (..)) where

data ResolveTargetModulePathConfig = ResolveTargetModulePathConfig
  { resolveTargetModulePathCurrentPackage :: [String],
    resolveTargetModulePathRelativeLevel :: Int,
    resolveTargetModulePathModulePath :: [String]
  }
