module PythonHS.Evaluator.EvalCallExprInput (EvalCallExprInput (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalExprResult (EvalExprResult)
import PythonHS.Evaluator.EvalStmtResult (EvalStmtResult)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Lexer.Position (Position)

data EvalCallExprInput = EvalCallExprInput
  { evalCallExprInputEvalStatementsFn :: Env -> FuncEnv -> [String] -> [Stmt] -> Either String EvalStmtResult,
    evalCallExprInputEvalExpr :: Env -> FuncEnv -> Expr -> Either String EvalExprResult,
    evalCallExprInputEnv :: Env,
    evalCallExprInputFuncEnv :: FuncEnv,
    evalCallExprInputName :: String,
    evalCallExprInputArgs :: [Expr],
    evalCallExprInputPos :: Position
  }
