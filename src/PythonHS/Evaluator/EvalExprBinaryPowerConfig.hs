module PythonHS.Evaluator.EvalExprBinaryPowerConfig (EvalExprBinaryPowerConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalExprResult (EvalExprResult)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data EvalExprBinaryPowerConfig = EvalExprBinaryPowerConfig
  { evalExprBinaryPowerConfigEvalExprFn :: Env -> FuncEnv -> Expr -> Either String EvalExprResult,
    evalExprBinaryPowerConfigEnv :: Env,
    evalExprBinaryPowerConfigFuncEnv :: FuncEnv,
    evalExprBinaryPowerConfigLeftExpr :: Expr,
    evalExprBinaryPowerConfigRightExpr :: Expr,
    evalExprBinaryPowerConfigPos :: Position,
    evalExprBinaryPowerConfigExpectNumber :: Position -> Value -> Either String Double
  }
