module PythonHS.Evaluator.EvalWhileStmtConfig (EvalWhileStmtConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalExprResult (EvalExprResult)
import PythonHS.Evaluator.EvalStmtResult (EvalStmtResult)
import PythonHS.Evaluator.FuncEnv (FuncEnv)

-- | Configuration for evaluating a while statement.
data EvalWhileStmtConfig = EvalWhileStmtConfig
  { evalWhileStmtEvalStatements :: Env -> FuncEnv -> [String] -> [Stmt] -> Either String EvalStmtResult,
    evalWhileStmtEvalExpr :: Env -> FuncEnv -> Expr -> Either String EvalExprResult
  }
