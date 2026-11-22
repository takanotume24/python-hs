module PythonHS.VM.SliceRangeResult (SliceRangeResult (..)) where

data SliceRangeResult = SliceRangeResult
  { sliceRangeResultStart :: Int,
    sliceRangeResultEnd :: Int
  }
