module PythonHS.Evaluator.EvalBuiltinExprInput (EvalBuiltinExprInput (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalExprResult (EvalExprResult)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Lexer.Position (Position)

data EvalBuiltinExprInput = EvalBuiltinExprInput
  { evalBuiltinExprEvalExprFn :: Env -> FuncEnv -> Expr -> Either String EvalExprResult,
    evalBuiltinExprEnv :: Env,
    evalBuiltinExprFuncEnv :: FuncEnv,
    evalBuiltinExprFname :: String,
    evalBuiltinExprArgs :: [Expr],
    evalBuiltinExprPos :: Position
  }
