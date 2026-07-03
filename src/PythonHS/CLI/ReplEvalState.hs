module PythonHS.CLI.ReplEvalState (ReplEvalState (..)) where

import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.FuncEnv (FuncEnv)

-- | State for a single REPL evaluation cycle.
data ReplEvalState = ReplEvalState
  { replEnv :: Env,
    replFunctionEnv :: FuncEnv,
    replOutputs :: [String]
  }
