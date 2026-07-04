module PythonHS.Evaluator.ExitContextManagerWithExceptionInput (ExitContextManagerWithExceptionInput (..)) where

import PythonHS.AST.ContextManager (ContextManager)
import PythonHS.AST.Expr (Expr)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalExprResult (EvalExprResult)
import PythonHS.Evaluator.FuncEnv (FuncEnv)

data ExitContextManagerWithExceptionInput = ExitContextManagerWithExceptionInput
  { exitContextManagerWithExceptionEvalExprFn :: Env -> FuncEnv -> Expr -> Either String EvalExprResult,
    exitContextManagerWithExceptionEnv :: Env,
    exitContextManagerWithExceptionFuncEnv :: FuncEnv,
    exitContextManagerWithExceptionContextManager :: ContextManager,
    exitContextManagerWithExceptionErr :: String
  }
