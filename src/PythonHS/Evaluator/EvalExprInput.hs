module PythonHS.Evaluator.EvalExprInput (EvalExprInput (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalStmtResult (EvalStmtResult)
import PythonHS.Evaluator.FuncEnv (FuncEnv)

data EvalExprInput = EvalExprInput
  { evalExprInputEvalStatementsFn :: Env -> FuncEnv -> [String] -> [Stmt] -> Either String EvalStmtResult,
    evalExprInputEnv :: Env,
    evalExprInputFuncEnv :: FuncEnv,
    evalExprInputExpr :: Expr
  }
