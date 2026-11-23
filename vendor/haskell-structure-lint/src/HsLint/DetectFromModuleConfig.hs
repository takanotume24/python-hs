module HsLint.DetectFromModuleConfig (DetectFromModuleConfig (..)) where

import HsLint.DetectModuleConfig (DetectModuleConfig)

-- | Configuration for detecting positional arguments from a module.
data DetectFromModuleConfig = DetectFromModuleConfig
  { detectFromModuleModuleConfig :: DetectModuleConfig,
    detectFromModuleSkipFunctionPaths :: [String]
  }
  deriving (Eq, Show)
