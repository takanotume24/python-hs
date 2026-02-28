module PythonHS.Evaluator.FuncEnv (FuncEnv) where

import Data.Map.Strict (Map)
import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt)

type FuncEnv = Map String ([String], [(String, Expr)], [Stmt])
