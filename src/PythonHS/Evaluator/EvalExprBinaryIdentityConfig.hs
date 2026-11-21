module PythonHS.Evaluator.EvalExprBinaryIdentityConfig (EvalExprBinaryIdentityConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalExprResult (EvalExprResult)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Lexer.Position (Position)

data EvalExprBinaryIdentityConfig = EvalExprBinaryIdentityConfig
  { evalExprBinaryIdentityConfigEvalExprFn :: Env -> FuncEnv -> Expr -> Either String EvalExprResult,
    evalExprBinaryIdentityConfigEnv :: Env,
    evalExprBinaryIdentityConfigFuncEnv :: FuncEnv,
    evalExprBinaryIdentityConfigLeftExpr :: Expr,
    evalExprBinaryIdentityConfigRightExpr :: Expr,
    evalExprBinaryIdentityConfigNegate :: Bool,
    evalExprBinaryIdentityConfigPos :: Position
  }
