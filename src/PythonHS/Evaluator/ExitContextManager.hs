module PythonHS.Evaluator.ExitContextManager (exitContextManager) where

import PythonHS.AST.ContextManager (ContextManager (..))
import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.WithExit (WithExit (..))
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalExprResult (EvalExprResult)
import PythonHS.Evaluator.ExitContextManagerInput (ExitContextManagerInput (..))
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.VM.Instruction (Instruction (..))

exitContextManager :: ExitContextManagerInput -> Either String EvalExprResult
exitContextManager input =
  let evalExprFn = exitContextManagerEvalExprFn input
      env = exitContextManagerEnv input
      fenv = exitContextManagerFuncEnv input
      contextManager = exitContextManagerContextManager input
      noneExpr = NoneExpr {noneExprPos = contextManagerPos contextManager}
      exitCall = CallExpr {callExprName = "__exit__", callExprArgs = [contextManagerExpr contextManager, noneExpr, noneExpr, noneExpr], callExprPos = contextManagerPos contextManager}
      exitInstruction = CallFunction {callFunctionName = "__exit__", callFunctionArgs = [], callFunctionPos = contextManagerPos contextManager}
      exitNormal = WithExit {exitCallExpr = exitCall, exitCallInstruction = exitInstruction, exitPos = contextManagerPos contextManager, exitIsException = False}
   in evalExprFn env fenv (exitCallExpr exitNormal)
