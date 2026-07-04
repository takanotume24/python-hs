module PythonHS.Structure.ExtractConAppResult (ExtractConAppResult (..)) where

import Language.Haskell.Exts (SrcSpanInfo)

-- | Result of extracting a constructor application from an expression.
data ExtractConAppResult conName = ExtractConAppResult
  { extractConAppResultName :: conName,
    extractConAppResultCount :: Int,
    extractConAppResultSpan :: SrcSpanInfo
  }
  deriving (Eq, Show)
