module PythonHS.Evaluator.EnterContextManager (enterContextManager) where

import PythonHS.AST.ContextManager (ContextManager (..))
import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.WithEntry (WithEntry (..))
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalExprResult (EvalExprResult)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.VM.Instruction (Instruction (..))

enterContextManager ::
  (Env -> FuncEnv -> Expr -> Either String EvalExprResult) ->
  Env ->
  FuncEnv ->
  ContextManager ->
  Either String EvalExprResult
enterContextManager evalExprFn env fenv contextManager = do
  let entryCall = CallExpr {callExprName = "__enter__", callExprArgs = [contextManagerExpr contextManager], callExprPos = contextManagerPos contextManager}
  let entryInstruction = CallFunction {callFunctionName = "__enter__", callFunctionArgs = [], callFunctionPos = contextManagerPos contextManager}
  let withEntry = WithEntry {entryCallExpr = entryCall, entryCallInstruction = entryInstruction, entryPos = contextManagerPos contextManager}
  evalExprFn env fenv (entryCallExpr withEntry)
