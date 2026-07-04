module PythonHS.CLI.ProcessSubmissionConfig (ProcessSubmissionConfig (..)) where

import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.FuncEnv (FuncEnv)

data ProcessSubmissionConfig = ProcessSubmissionConfig
  { processSubmissionEnv :: Env,
    processSubmissionFuncEnv :: FuncEnv,
    processSubmissionSrc :: String
  }
