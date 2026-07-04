module PythonHS.Evaluator.RangeValues (rangeValues) where

import PythonHS.Evaluator.RangeValuesConfig (RangeValuesConfig (..))

rangeValues :: RangeValuesConfig -> [Integer]
rangeValues config =
  let start = rangeValuesStart config
      stop = rangeValuesStop config
      step = rangeValuesStep config
   in case () of
        _
          | step > 0 -> takeWhile (< stop) [start, start + step ..]
          | step < 0 -> takeWhile (> stop) [start, start + step ..]
          | otherwise -> []
