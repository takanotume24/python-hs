module PythonHS.CLI.SubmissionResult (SubmissionResult (..)) where

import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.FuncEnv (FuncEnv)

-- | Result of processing an AST REPL submission.
data SubmissionResult = SubmissionResult
  { submissionEnv :: Env,
    submissionFuncEnv :: FuncEnv,
    submissionOutputs :: [String]
  }
