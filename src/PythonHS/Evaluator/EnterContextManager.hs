module PythonHS.Evaluator.EnterContextManager (enterContextManager) where

import PythonHS.AST.Expr (Expr (CallExpr))
import PythonHS.AST.ContextManager (ContextManager(..))
import PythonHS.AST.WithEntry (WithEntry(..))
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Evaluator.Value (Value)
import PythonHS.VM.Instruction (Instruction(CallFunction))

enterContextManager ::
  (Env -> FuncEnv -> Expr -> Either String (Value, [String], Env)) ->
  Env ->
  FuncEnv ->
  ContextManager ->
  Either String (Value, [String], Env)
enterContextManager evalExprFn env fenv contextManager = do
  let entryCall = CallExpr "__enter__" [contextManagerExpr contextManager] (contextManagerPos contextManager)
  let entryInstruction = CallFunction "__enter__" [] (contextManagerPos contextManager)
  let withEntry = WithEntry entryCall entryInstruction (contextManagerPos contextManager)
  evalExprFn env fenv (entryCallExpr withEntry)
