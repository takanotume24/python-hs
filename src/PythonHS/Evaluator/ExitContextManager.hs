module PythonHS.Evaluator.ExitContextManager (exitContextManager) where

import PythonHS.AST.ContextManager (ContextManager (..))
import PythonHS.AST.Expr (Expr (CallExpr, NoneExpr))
import PythonHS.AST.WithExit (WithExit (..))
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalExprResult (EvalExprResult)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.VM.Instruction (Instruction (CallFunction))

exitContextManager ::
  (Env -> FuncEnv -> Expr -> Either String EvalExprResult) ->
  Env ->
  FuncEnv ->
  ContextManager ->
  Either String EvalExprResult
exitContextManager evalExprFn env fenv contextManager = do
  let noneExpr = NoneExpr (contextManagerPos contextManager)
  let exitCall = CallExpr "__exit__" [contextManagerExpr contextManager, noneExpr, noneExpr, noneExpr] (contextManagerPos contextManager)
  let exitInstruction = CallFunction "__exit__" [] (contextManagerPos contextManager)
  let exitNormal = WithExit exitCall exitInstruction (contextManagerPos contextManager) False
  evalExprFn env fenv (exitCallExpr exitNormal)
