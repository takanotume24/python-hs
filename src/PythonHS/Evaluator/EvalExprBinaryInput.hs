module PythonHS.Evaluator.EvalExprBinaryInput (EvalExprBinaryInput (..)) where

import PythonHS.AST.BinaryOperator (BinaryOperator)
import PythonHS.AST.Expr (Expr)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalExprResult (EvalExprResult)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Lexer.Position (Position)

data EvalExprBinaryInput = EvalExprBinaryInput
  { evalExprBinaryEvalExprFn :: Env -> FuncEnv -> Expr -> Either String EvalExprResult,
    evalExprBinaryEnv :: Env,
    evalExprBinaryFuncEnv :: FuncEnv,
    evalExprBinaryOp :: BinaryOperator,
    evalExprBinaryLeftExpr :: Expr,
    evalExprBinaryRightExpr :: Expr,
    evalExprBinaryPos :: Position
  }
