module PythonHS.Evaluator.EvalWithStmtConfig (EvalWithStmtConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalExprResult (EvalExprResult)
import PythonHS.Evaluator.EvalStmtResult (EvalStmtResult)
import PythonHS.Evaluator.FuncEnv (FuncEnv)

-- | Configuration for evaluating a with statement.
data EvalWithStmtConfig = EvalWithStmtConfig
  { evalWithStmtEvalStatements :: Env -> FuncEnv -> [String] -> [Stmt] -> Either String EvalStmtResult,
    evalWithStmtEvalExpr :: Env -> FuncEnv -> Expr -> Either String EvalExprResult
  }
