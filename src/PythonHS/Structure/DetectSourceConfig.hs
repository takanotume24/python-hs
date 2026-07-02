module PythonHS.Structure.DetectSourceConfig (DetectSourceConfig (..)) where

-- | Configuration for detecting positional arguments in source text.
data DetectSourceConfig = DetectSourceConfig
  { sourceFilePath :: FilePath,
    sourceContent :: String
  }
  deriving (Eq, Show)
