module PythonHS.Evaluator.EvalDictEntriesInput (EvalDictEntriesInput (..)) where

import PythonHS.AST.DictEntry (DictEntry)
import PythonHS.AST.Expr (Expr)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.FuncEnv (FuncEnv)

data EvalDictEntriesInput = EvalDictEntriesInput
  { evalDictEntriesInputEnv :: Env,
    evalDictEntriesInputFuncEnv :: FuncEnv,
    evalDictEntriesInputEntries :: [DictEntry Expr]
  }
