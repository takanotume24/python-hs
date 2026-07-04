module PythonHS.Evaluator.EvalWithStmtInput (EvalWithStmtInput (..)) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Evaluator.EvalWithStmtConfig (EvalWithStmtConfig)
import PythonHS.Lexer.Position (Position)

data EvalWithStmtInput = EvalWithStmtInput
  { evalWithStmtInputConfig :: EvalWithStmtConfig,
    evalWithStmtInputEnv :: Env,
    evalWithStmtInputFuncEnv :: FuncEnv,
    evalWithStmtInputOutputs :: [String],
    evalWithStmtInputContextManager :: Expr,
    evalWithStmtInputMaybeVarName :: Maybe String,
    evalWithStmtInputBody :: [Stmt],
    evalWithStmtInputPos :: Position,
    evalWithStmtInputRest :: [Stmt]
  }
