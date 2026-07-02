module PythonHS.Structure.ExemptFileInfo (ExemptFileInfo (..)) where

-- | Information about a file that has a temporary line-count exemption.
data ExemptFileInfo = ExemptFileInfo
  { filePathExempt :: FilePath,
    lineCountExempt :: Int
  }
  deriving (Eq, Show)
