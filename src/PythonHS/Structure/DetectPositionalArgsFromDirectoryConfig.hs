module PythonHS.Structure.DetectPositionalArgsFromDirectoryConfig (DetectPositionalArgsFromDirectoryConfig (..)) where

-- | Configuration for detecting positional arguments from a directory.
data DetectPositionalArgsFromDirectoryConfig = DetectPositionalArgsFromDirectoryConfig
  { detectPositionalArgsFromDirectoryDir :: FilePath,
    detectPositionalArgsFromDirectoryExcludes :: [String]
  }
