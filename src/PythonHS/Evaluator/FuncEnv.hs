module PythonHS.Evaluator.FuncEnv (FuncEnv) where

import Data.Map.Strict (Map)
import PythonHS.AST.Stmt (Stmt)

type FuncEnv = Map String ([String], [Stmt])
