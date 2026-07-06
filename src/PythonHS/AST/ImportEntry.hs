module PythonHS.AST.ImportEntry (ImportEntry (..)) where

data ImportEntry = ImportEntry
  { importEntryPath :: [String],
    importEntryAlias :: Maybe String
  }
  deriving (Eq, Show)
