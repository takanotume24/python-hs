module PythonHS.CLI.ReplEnvState (ReplEnvState (..)) where

import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.FuncEnv (FuncEnv)

-- | Combined environment state for the AST REPL loop.
data ReplEnvState = ReplEnvState
  { replEnvStateEnv :: Env,
    replEnvStateFuncEnv :: FuncEnv
  }
