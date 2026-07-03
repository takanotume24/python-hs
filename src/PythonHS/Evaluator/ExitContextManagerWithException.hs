module PythonHS.Evaluator.ExitContextManagerWithException (exitContextManagerWithException) where

import PythonHS.AST.Expr (Expr (CallExpr, NoneExpr, StringExpr))
import PythonHS.AST.ContextManager (ContextManager(..))
import PythonHS.AST.WithExit (WithExit(..))
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Evaluator.EvalExprResult (EvalExprResult)
import PythonHS.VM.Instruction (Instruction(CallFunction))

exitContextManagerWithException ::
  (Env -> FuncEnv -> Expr -> Either String EvalExprResult) ->
  Env ->
  FuncEnv ->
  ContextManager ->
  String ->
  Either String EvalExprResult
exitContextManagerWithException evalExprFn env fenv contextManager err = do
  let withPos = contextManagerPos contextManager
  let exitCall = CallExpr "__exit__" [contextManagerExpr contextManager, StringExpr "Exception" withPos, StringExpr err withPos, NoneExpr withPos] withPos
  let exitInstruction = CallFunction "__exit__" [] withPos
  let exitException = WithExit exitCall exitInstruction withPos True
  evalExprFn env fenv (exitCallExpr exitException)
