module PythonHS.Evaluator.EvalExprResult (EvalExprResult (..)) where

import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.Value (Value)

-- | Result of evaluating an expression.
data EvalExprResult = EvalExprResult
  { evalExprResultValue :: Value,
    evalExprResultOutputs :: [String],
    evalExprResultEnv :: Env
  }
  deriving (Eq, Show)
