module PythonHS.Evaluator.RangeOne (rangeOne) where

rangeOne :: Integer -> [Integer]
rangeOne n
  | n <= 0 = []
  | otherwise = [0 .. n - 1]
