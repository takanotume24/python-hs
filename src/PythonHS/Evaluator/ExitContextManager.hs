module PythonHS.Evaluator.ExitContextManager (exitContextManager) where

import PythonHS.AST.ContextManager (ContextManager (..))
import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.WithExit (WithExit (..))
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalExprResult (EvalExprResult)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.VM.Instruction (Instruction (..))

exitContextManager ::
  (Env -> FuncEnv -> Expr -> Either String EvalExprResult) ->
  Env ->
  FuncEnv ->
  ContextManager ->
  Either String EvalExprResult
exitContextManager evalExprFn env fenv contextManager = do
  let noneExpr = NoneExpr {noneExprPos = contextManagerPos contextManager}
  let exitCall = CallExpr {callExprName = "__exit__", callExprArgs = [contextManagerExpr contextManager, noneExpr, noneExpr, noneExpr], callExprPos = contextManagerPos contextManager}
  let exitInstruction = CallFunction {callFunctionName = "__exit__", callFunctionArgs = [], callFunctionPos = contextManagerPos contextManager}
  let exitNormal = WithExit {exitCallExpr = exitCall, exitCallInstruction = exitInstruction, exitPos = contextManagerPos contextManager, exitIsException = False}
  evalExprFn env fenv (exitCallExpr exitNormal)
