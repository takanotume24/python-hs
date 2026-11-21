module PythonHS.Evaluator.EvalArgsInput (EvalArgsInput (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalExprResult (EvalExprResult)
import PythonHS.Evaluator.FuncEnv (FuncEnv)

data EvalArgsInput = EvalArgsInput
  { evalArgsInputEvalExpr :: Env -> FuncEnv -> Expr -> Either String EvalExprResult,
    evalArgsInputEnv :: Env,
    evalArgsInputFuncEnv :: FuncEnv,
    evalArgsInputExprs :: [Expr]
  }
