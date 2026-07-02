module PythonHS.Structure.DetectModuleConfig (DetectModuleConfig (..)) where

import Language.Haskell.Exts (Module, SrcSpanInfo)

-- | Configuration for detecting positional arguments from an AST module.
data DetectModuleConfig = DetectModuleConfig
  { moduleFilePath :: FilePath,
    moduleAst :: Module SrcSpanInfo
  }
  deriving (Eq, Show)
