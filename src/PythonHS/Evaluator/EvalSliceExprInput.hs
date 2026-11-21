module PythonHS.Evaluator.EvalSliceExprInput (EvalSliceExprInput (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalExprResult (EvalExprResult)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Lexer.Position (Position)

data EvalSliceExprInput = EvalSliceExprInput
  { evalSliceExprInputEvalExpr :: Env -> FuncEnv -> Expr -> Either String EvalExprResult,
    evalSliceExprInputEnv :: Env,
    evalSliceExprInputFuncEnv :: FuncEnv,
    evalSliceExprInputBase :: Expr,
    evalSliceExprInputStart :: Maybe Expr,
    evalSliceExprInputEnd :: Maybe Expr,
    evalSliceExprInputPos :: Position
  }
