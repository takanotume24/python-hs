module PythonHS.Evaluator.EvalStmtResult (EvalStmtResult (..)) where

import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalStmtRet (EvalStmtRet)
import PythonHS.Evaluator.FuncEnv (FuncEnv)

data EvalStmtResult = EvalStmtResult
  { evalStmtResultEnv :: Env,
    evalStmtResultFuncEnv :: FuncEnv,
    evalStmtResultOutputs :: [String],
    evalStmtResultRet :: Maybe EvalStmtRet
  }
  deriving (Eq, Show)
