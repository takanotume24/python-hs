module PythonHS.Evaluator.ExitContextManagerWithException (exitContextManagerWithException) where

import PythonHS.AST.ContextManager (ContextManager (..))
import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.WithExit (WithExit (..))
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalExprResult (EvalExprResult)
import PythonHS.Evaluator.ExitContextManagerWithExceptionInput (ExitContextManagerWithExceptionInput (..))
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.VM.Instruction (Instruction (..))

exitContextManagerWithException :: ExitContextManagerWithExceptionInput -> Either String EvalExprResult
exitContextManagerWithException input =
  let evalExprFn = exitContextManagerWithExceptionEvalExprFn input
      env = exitContextManagerWithExceptionEnv input
      fenv = exitContextManagerWithExceptionFuncEnv input
      contextManager = exitContextManagerWithExceptionContextManager input
      err = exitContextManagerWithExceptionErr input
      withPos = contextManagerPos contextManager
      exitCall = CallExpr {callExprName = "__exit__", callExprArgs = [contextManagerExpr contextManager, StringExpr {stringExprValue = "Exception", stringExprPos = withPos}, StringExpr {stringExprValue = err, stringExprPos = withPos}, NoneExpr {noneExprPos = withPos}], callExprPos = withPos}
      exitInstruction = CallFunction {callFunctionName = "__exit__", callFunctionArgs = [], callFunctionPos = withPos}
      exitException = WithExit {exitCallExpr = exitCall, exitCallInstruction = exitInstruction, exitPos = withPos, exitIsException = True}
   in evalExprFn env fenv (exitCallExpr exitException)
