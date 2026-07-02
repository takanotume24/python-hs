{-# LANGUAGE OverloadedStrings #-}

module PythonHS.Structure.PositionalArgViolation
  ( PositionalArgViolation (..),
  )
where

import Data.Aeson (ToJSON (..), object, (.=))
import PythonHS.Structure.ViolationCategory (ViolationCategory)

-- | A single detected violation.
data PositionalArgViolation = PositionalArgViolation
  { filePath :: FilePath,
    line :: Int,
    column :: Int,
    category :: ViolationCategory,
    snippet :: String
  }
  deriving (Eq, Show)

instance ToJSON PositionalArgViolation where
  toJSON v =
    object
      [ "file" .= filePath v,
        "line" .= line v,
        "column" .= column v,
        "category" .= category v,
        "snippet" .= snippet v
      ]
