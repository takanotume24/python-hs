module PythonHS.Evaluator.RangeValues (rangeValues) where

rangeValues :: Integer -> Integer -> Integer -> [Integer]
rangeValues start stop step
  | step > 0 = takeWhile (< stop) [start, start + step ..]
  | step < 0 = takeWhile (> stop) [start, start + step ..]
  | otherwise = []