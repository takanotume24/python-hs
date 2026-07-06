module PythonHS.AST.MappingPatternEntry (MappingPatternEntry (..)) where

data MappingPatternEntry e p = MappingPatternEntry
  { mappingPatternEntryKey :: e,
    mappingPatternEntryValue :: p
  }
  deriving (Eq, Show)
