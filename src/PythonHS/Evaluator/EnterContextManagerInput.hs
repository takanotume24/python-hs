module PythonHS.Evaluator.EnterContextManagerInput (EnterContextManagerInput (..)) where

import PythonHS.AST.ContextManager (ContextManager)
import PythonHS.AST.Expr (Expr)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalExprResult (EvalExprResult)
import PythonHS.Evaluator.FuncEnv (FuncEnv)

data EnterContextManagerInput = EnterContextManagerInput
  { enterContextManagerEvalExprFn :: Env -> FuncEnv -> Expr -> Either String EvalExprResult,
    enterContextManagerEnv :: Env,
    enterContextManagerFuncEnv :: FuncEnv,
    enterContextManagerContextManager :: ContextManager
  }
