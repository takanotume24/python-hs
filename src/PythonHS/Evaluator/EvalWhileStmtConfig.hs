module PythonHS.Evaluator.EvalWhileStmtConfig (EvalWhileStmtConfig (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

-- | Configuration for evaluating a while statement.
data EvalWhileStmtConfig = EvalWhileStmtConfig
  { evalWhileStmtEvalStatements :: Env -> FuncEnv -> [String] -> [Stmt] -> Either String (Env, FuncEnv, [String], Maybe (Value, Position)),
    evalWhileStmtEvalExpr :: Env -> FuncEnv -> Expr -> Either String (Value, [String], Env)
  }
