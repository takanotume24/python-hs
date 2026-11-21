module PythonHS.Evaluator.EvalForStmtInput (EvalForStmtInput (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalForStmtConfig (EvalForStmtConfig)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Lexer.Position (Position)

data EvalForStmtInput = EvalForStmtInput
  { evalForStmtInputConfig :: EvalForStmtConfig,
    evalForStmtInputEnv :: Env,
    evalForStmtInputFuncEnv :: FuncEnv,
    evalForStmtInputOutputs :: [String],
    evalForStmtInputName :: String,
    evalForStmtInputIterExpr :: Expr,
    evalForStmtInputBody :: [Stmt],
    evalForStmtInputPos :: Position,
    evalForStmtInputRest :: [Stmt]
  }
