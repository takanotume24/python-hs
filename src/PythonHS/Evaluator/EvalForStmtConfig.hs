module PythonHS.Evaluator.EvalForStmtConfig (EvalForStmtConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalExprResult (EvalExprResult)
import PythonHS.Evaluator.EvalStmtResult (EvalStmtResult)
import PythonHS.Evaluator.FuncEnv (FuncEnv)

-- | Configuration for evaluating a for statement.
data EvalForStmtConfig = EvalForStmtConfig
  { evalForStmtEvalStatements :: Env -> FuncEnv -> [String] -> [Stmt] -> Either String EvalStmtResult,
    evalForStmtEvalExpr :: Env -> FuncEnv -> Expr -> Either String EvalExprResult
  }
