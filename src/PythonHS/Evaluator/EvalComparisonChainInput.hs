module PythonHS.Evaluator.EvalComparisonChainInput (EvalComparisonChainInput (..)) where

import PythonHS.AST.BinaryOperator (BinaryOperator)
import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalExprResult (EvalExprResult)
import PythonHS.Evaluator.EvalStmtResult (EvalStmtResult)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Lexer.Position (Position)

data EvalComparisonChainInput = EvalComparisonChainInput
  { evalComparisonChainInputEvalStatementsFn :: Env -> FuncEnv -> [String] -> [Stmt] -> Either String EvalStmtResult,
    evalComparisonChainInputEvalExpr :: Env -> FuncEnv -> Expr -> Either String EvalExprResult,
    evalComparisonChainInputEnv :: Env,
    evalComparisonChainInputFuncEnv :: FuncEnv,
    evalComparisonChainInputLeft :: Expr,
    evalComparisonChainInputOps :: [BinaryOperator],
    evalComparisonChainInputRights :: [Expr],
    evalComparisonChainInputPos :: Position
  }
