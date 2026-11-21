module PythonHS.Evaluator.EvalComparisonItemsInput (EvalComparisonItemsInput (..)) where

import PythonHS.AST.BinaryOperator (BinaryOperator)
import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalExprResult (EvalExprResult)
import PythonHS.Evaluator.EvalStmtResult (EvalStmtResult)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data EvalComparisonItemsInput = EvalComparisonItemsInput
  { evalComparisonItemsInputEvalStatementsFn :: Env -> FuncEnv -> [String] -> [Stmt] -> Either String EvalStmtResult,
    evalComparisonItemsInputEvalExpr :: Env -> FuncEnv -> Expr -> Either String EvalExprResult,
    evalComparisonItemsInputFuncEnv :: FuncEnv,
    evalComparisonItemsInputLeftValue :: Value,
    evalComparisonItemsInputAccOutputs :: [String],
    evalComparisonItemsInputEnv :: Env,
    evalComparisonItemsInputRemainingOps :: [BinaryOperator],
    evalComparisonItemsInputRemainingRights :: [Expr],
    evalComparisonItemsInputPos :: Position
  }
