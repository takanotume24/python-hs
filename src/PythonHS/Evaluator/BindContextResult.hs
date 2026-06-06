module PythonHS.Evaluator.BindContextResult (bindContextResult) where

import qualified Data.Map.Strict as Map
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.Value (Value)

bindContextResult :: Maybe String -> Value -> Env -> Env
bindContextResult maybeVarName enterValue env =
  case maybeVarName of
    Just varName -> Map.insert varName enterValue env
    Nothing -> env
