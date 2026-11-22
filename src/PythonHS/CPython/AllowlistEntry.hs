module PythonHS.CPython.AllowlistEntry (AllowlistEntry (..)) where

-- | A single allowlist entry before it is expanded into concrete test cases.
--
-- Entries may name a file, a class, or a single method. Class and file entries
-- are expanded by scanning the referenced source file for @test*@ methods.
data AllowlistEntry = AllowlistEntry
  { allowlistEntryFile :: FilePath,
    allowlistEntryClassName :: Maybe String,
    allowlistEntryMethodName :: Maybe String
  }
  deriving (Eq, Show)
