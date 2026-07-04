module PythonHS.Evaluator.EvalWhileStmtInput (EvalWhileStmtInput (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Evaluator.EvalWhileStmtConfig (EvalWhileStmtConfig)
import PythonHS.Lexer.Position (Position)

data EvalWhileStmtInput = EvalWhileStmtInput
  { evalWhileStmtInputConfig :: EvalWhileStmtConfig,
    evalWhileStmtInputEnv :: Env,
    evalWhileStmtInputFuncEnv :: FuncEnv,
    evalWhileStmtInputOutputs :: [String],
    evalWhileStmtInputCond :: Expr,
    evalWhileStmtInputBody :: [Stmt],
    evalWhileStmtInputPos :: Position,
    evalWhileStmtInputRest :: [Stmt]
  }
