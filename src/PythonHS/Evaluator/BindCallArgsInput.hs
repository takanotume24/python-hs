module PythonHS.Evaluator.BindCallArgsInput (BindCallArgsInput (..)) where

import Data.Map.Strict (Map)
import PythonHS.AST.Expr (Expr)
import PythonHS.AST.FunctionDefault (FunctionDefault)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalExprResult (EvalExprResult)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)

data BindCallArgsInput = BindCallArgsInput
  { bindCallArgsInputEvalExpr :: Env -> FuncEnv -> Expr -> Either String EvalExprResult,
    bindCallArgsInputEnv :: Env,
    bindCallArgsInputFuncEnv :: FuncEnv,
    bindCallArgsInputParams :: [String],
    bindCallArgsInputDefaults :: [FunctionDefault Expr],
    bindCallArgsInputPosVals :: [Value],
    bindCallArgsInputKwVals :: Map String Value,
    bindCallArgsInputKwPosByName :: Map String Position,
    bindCallArgsInputKwOrder :: [String],
    bindCallArgsInputFunctionName :: String,
    bindCallArgsInputPos :: Position
  }
