module PythonHS.Structure.DetectFromModuleConfig (DetectFromModuleConfig (..)) where

import PythonHS.Structure.DetectModuleConfig (DetectModuleConfig)

-- | Configuration for detecting positional arguments from a module.
data DetectFromModuleConfig = DetectFromModuleConfig
  { detectFromModuleRecordConNames :: [String],
    detectFromModuleModuleConfig :: DetectModuleConfig
  }
  deriving (Eq, Show)
