module PythonHS.Evaluator.EvaluatedArgsResult (EvaluatedArgsResult (..)) where

import PythonHS.Evaluator.Env (Env)

data EvaluatedArgsResult a = EvaluatedArgsResult
  { evaluatedArgsResultValues :: [a],
    evaluatedArgsResultOutputs :: [String],
    evaluatedArgsResultEnv :: Env
  }
  deriving (Eq, Show)
