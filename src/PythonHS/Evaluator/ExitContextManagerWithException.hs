module PythonHS.Evaluator.ExitContextManagerWithException (exitContextManagerWithException) where

import PythonHS.AST.ContextManager (ContextManager (..))
import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.WithExit (WithExit (..))
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalExprResult (EvalExprResult)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.VM.Instruction (Instruction (..))

exitContextManagerWithException ::
  (Env -> FuncEnv -> Expr -> Either String EvalExprResult) ->
  Env ->
  FuncEnv ->
  ContextManager ->
  String ->
  Either String EvalExprResult
exitContextManagerWithException evalExprFn env fenv contextManager err = do
  let withPos = contextManagerPos contextManager
  let exitCall = CallExpr {callExprName = "__exit__", callExprArgs = [contextManagerExpr contextManager, StringExpr {stringExprValue = "Exception", stringExprPos = withPos}, StringExpr {stringExprValue = err, stringExprPos = withPos}, NoneExpr {noneExprPos = withPos}], callExprPos = withPos}
  let exitInstruction = CallFunction {callFunctionName = "__exit__", callFunctionArgs = [], callFunctionPos = withPos}
  let exitException = WithExit {exitCallExpr = exitCall, exitCallInstruction = exitInstruction, exitPos = withPos, exitIsException = True}
  evalExprFn env fenv (exitCallExpr exitException)
