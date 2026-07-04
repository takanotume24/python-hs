module PythonHS.Evaluator.EnterContextManager (enterContextManager) where

import PythonHS.AST.ContextManager (ContextManager (..))
import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.WithEntry (WithEntry (..))
import PythonHS.Evaluator.EnterContextManagerInput (EnterContextManagerInput (..))
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalExprResult (EvalExprResult)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.VM.Instruction (Instruction (..))

enterContextManager :: EnterContextManagerInput -> Either String EvalExprResult
enterContextManager input =
  let evalExprFn = enterContextManagerEvalExprFn input
      env = enterContextManagerEnv input
      fenv = enterContextManagerFuncEnv input
      contextManager = enterContextManagerContextManager input
      entryCall = CallExpr {callExprName = "__enter__", callExprArgs = [contextManagerExpr contextManager], callExprPos = contextManagerPos contextManager}
      entryInstruction = CallFunction {callFunctionName = "__enter__", callFunctionArgs = [], callFunctionPos = contextManagerPos contextManager}
      withEntry = WithEntry {entryCallExpr = entryCall, entryCallInstruction = entryInstruction, entryPos = contextManagerPos contextManager}
   in evalExprFn env fenv (entryCallExpr withEntry)
