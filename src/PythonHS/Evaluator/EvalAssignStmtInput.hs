module PythonHS.Evaluator.EvalAssignStmtInput (EvalAssignStmtInput (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Evaluator.ComputeAssignInput (ComputeAssignInput (..))
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalExprResult (EvalExprResult)
import PythonHS.Evaluator.EvalStmtResult (EvalStmtResult)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data EvalAssignStmtInput = EvalAssignStmtInput
  { evalAssignStmtInputEvalExpr :: Expr -> Either String EvalExprResult,
    evalAssignStmtInputEvalStatements :: Env -> FuncEnv -> [String] -> [Stmt] -> Either String EvalStmtResult,
    evalAssignStmtInputEnv :: Env,
    evalAssignStmtInputFuncEnv :: FuncEnv,
    evalAssignStmtInputOutputs :: [String],
    evalAssignStmtInputRest :: [Stmt],
    evalAssignStmtInputName :: String,
    evalAssignStmtInputValueExpr :: Expr,
    evalAssignStmtInputPos :: Position,
    evalAssignStmtInputOp :: String,
    evalAssignStmtInputCompute :: ComputeAssignInput -> Either String Value
  }
