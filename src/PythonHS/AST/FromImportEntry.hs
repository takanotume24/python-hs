module PythonHS.AST.FromImportEntry (FromImportEntry (..)) where

data FromImportEntry = FromImportEntry
  { fromImportEntryName :: String,
    fromImportEntryAlias :: Maybe String
  }
  deriving (Eq, Show)
