module HsLint.DetectSourceConfig (DetectSourceConfig (..)) where

-- | Configuration for detecting positional arguments in source text.
data DetectSourceConfig = DetectSourceConfig
  { sourceFilePath :: FilePath,
    sourceContent :: String,
    sourceSkipFunctionPaths :: [String]
  }
  deriving (Eq, Show)
