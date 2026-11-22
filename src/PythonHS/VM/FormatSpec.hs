module PythonHS.VM.FormatSpec (FormatSpec (..)) where

-- | Parsed pieces of a Python format specifier.
data FormatSpec = FormatSpec
  { formatSpecFill :: Maybe Char,
    formatSpecAlign :: Maybe Char,
    formatSpecSign :: Maybe Char,
    formatSpecZeroPad :: Bool,
    formatSpecWidth :: Maybe Int,
    formatSpecThousands :: Bool,
    formatSpecPrecision :: Maybe Int,
    formatSpecType :: Maybe Char
  }
  deriving (Eq, Show)
