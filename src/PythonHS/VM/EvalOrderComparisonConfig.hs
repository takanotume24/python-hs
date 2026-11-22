module PythonHS.VM.EvalOrderComparisonConfig (EvalOrderComparisonConfig (..)) where

import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data EvalOrderComparisonConfig = EvalOrderComparisonConfig
  { evalOrderComparisonContext :: String,
    evalOrderComparisonPos :: Position,
    evalOrderComparisonLeft :: Value,
    evalOrderComparisonRight :: Value,
    evalOrderComparisonCompareResult :: Ordering -> Bool
  }
