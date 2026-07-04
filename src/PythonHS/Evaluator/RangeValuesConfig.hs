module PythonHS.Evaluator.RangeValuesConfig (RangeValuesConfig (..)) where

data RangeValuesConfig = RangeValuesConfig
  { rangeValuesStart :: Integer,
    rangeValuesStop :: Integer,
    rangeValuesStep :: Integer
  }
