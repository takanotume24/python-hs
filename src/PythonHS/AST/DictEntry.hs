module PythonHS.AST.DictEntry (DictEntry (..)) where

data DictEntry e = DictEntry
  { dictEntryKey :: e,
    dictEntryValue :: e
  }
  deriving (Eq, Show)
