module PythonHS.Evaluator.EvalExprBinaryBitwiseConfig (EvalExprBinaryBitwiseConfig (..)) where

import PythonHS.AST.BinaryOperator (BinaryOperator)
import PythonHS.AST.Expr (Expr)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalExprResult (EvalExprResult)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Lexer.Position (Position)

data EvalExprBinaryBitwiseConfig = EvalExprBinaryBitwiseConfig
  { evalExprBinaryBitwiseConfigEvalExprFn :: Env -> FuncEnv -> Expr -> Either String EvalExprResult,
    evalExprBinaryBitwiseConfigEnv :: Env,
    evalExprBinaryBitwiseConfigFuncEnv :: FuncEnv,
    evalExprBinaryBitwiseConfigLeftExpr :: Expr,
    evalExprBinaryBitwiseConfigRightExpr :: Expr,
    evalExprBinaryBitwiseConfigOp :: BinaryOperator,
    evalExprBinaryBitwiseConfigPos :: Position
  }
