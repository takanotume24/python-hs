module PythonHS.Evaluator.ExitContextManagerInput (ExitContextManagerInput (..)) where

import PythonHS.AST.ContextManager (ContextManager)
import PythonHS.AST.Expr (Expr)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalExprResult (EvalExprResult)
import PythonHS.Evaluator.FuncEnv (FuncEnv)

data ExitContextManagerInput = ExitContextManagerInput
  { exitContextManagerEvalExprFn :: Env -> FuncEnv -> Expr -> Either String EvalExprResult,
    exitContextManagerEnv :: Env,
    exitContextManagerFuncEnv :: FuncEnv,
    exitContextManagerContextManager :: ContextManager
  }
