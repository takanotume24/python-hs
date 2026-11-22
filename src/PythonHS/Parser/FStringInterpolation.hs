module PythonHS.Parser.FStringInterpolation (FStringInterpolation (..)) where

-- | One @{...}@ interpolation split into its expression, conversion flag,
-- format specifier, and the remaining f-string content.
data FStringInterpolation = FStringInterpolation
  { fStringInterpolationExpr :: String,
    fStringInterpolationConversion :: Maybe Char,
    fStringInterpolationSpec :: Maybe String,
    fStringInterpolationRemaining :: String
  }
  deriving (Eq, Show)
