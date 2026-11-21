module PythonHS.Evaluator.EvalExprBinaryMembershipConfig (EvalExprBinaryMembershipConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalExprResult (EvalExprResult)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Lexer.Position (Position)

data EvalExprBinaryMembershipConfig = EvalExprBinaryMembershipConfig
  { evalExprBinaryMembershipConfigEvalExprFn :: Env -> FuncEnv -> Expr -> Either String EvalExprResult,
    evalExprBinaryMembershipConfigEnv :: Env,
    evalExprBinaryMembershipConfigFuncEnv :: FuncEnv,
    evalExprBinaryMembershipConfigLeftExpr :: Expr,
    evalExprBinaryMembershipConfigRightExpr :: Expr,
    evalExprBinaryMembershipConfigPos :: Position,
    evalExprBinaryMembershipConfigNegate :: Bool
  }
