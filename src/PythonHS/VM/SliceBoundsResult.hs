module PythonHS.VM.SliceBoundsResult (SliceBoundsResult (..)) where

data SliceBoundsResult = SliceBoundsResult
  { sliceBoundsResultStart :: Int,
    sliceBoundsResultEnd :: Int
  }
  deriving (Eq, Show)
