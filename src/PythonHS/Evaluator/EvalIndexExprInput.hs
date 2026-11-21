module PythonHS.Evaluator.EvalIndexExprInput (EvalIndexExprInput (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalExprResult (EvalExprResult)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Lexer.Position (Position)

data EvalIndexExprInput = EvalIndexExprInput
  { evalIndexExprInputEvalExpr :: Env -> FuncEnv -> Expr -> Either String EvalExprResult,
    evalIndexExprInputEnv :: Env,
    evalIndexExprInputFuncEnv :: FuncEnv,
    evalIndexExprInputBase :: Expr,
    evalIndexExprInputIndex :: Expr,
    evalIndexExprInputPos :: Position
  }
