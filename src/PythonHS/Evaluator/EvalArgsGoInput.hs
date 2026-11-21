module PythonHS.Evaluator.EvalArgsGoInput (EvalArgsGoInput (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalExprResult (EvalExprResult)
import PythonHS.Evaluator.EvaluatedArgsResult (EvaluatedArgsResult)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Evaluator.Value (Value)

data EvalArgsGoInput = EvalArgsGoInput
  { evalArgsGoInputEvalExpr :: Env -> FuncEnv -> Expr -> Either String EvalExprResult,
    evalArgsGoInputFuncEnv :: FuncEnv,
    evalArgsGoInputAcc :: Either String (EvaluatedArgsResult Value),
    evalArgsGoInputExprs :: [Expr]
  }
