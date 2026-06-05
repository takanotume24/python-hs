module PythonHS.Evaluator.EvalWithStmt (evalWithStmt) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalContextManager (bindContextResult, enterContextManager, exitContextManager, exitContextManagerWithException)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Evaluator.Value (Value (IntValue))
import PythonHS.Lexer.Position (Position)

evalWithStmt ::
  (Env -> FuncEnv -> [String] -> [Stmt] -> Either String (Env, FuncEnv, [String], Maybe (Value, Position))) ->
  (Env -> FuncEnv -> Expr -> Either String (Value, [String], Env)) ->
  Env ->
  FuncEnv ->
  [String] ->
  Expr ->
  Maybe String ->
  [Stmt] ->
  Position ->
  [Stmt] ->
  Either String (Env, FuncEnv, [String], Maybe (Value, Position))
evalWithStmt evalStatementsFn evalExprFn env fenv outputs contextManager maybeVarName body withPos rest = do
  -- Evaluate the context manager expression
  (_, cmOuts, envAfterCM) <- evalExprFn env fenv contextManager
  
  -- Enter the context manager
  (enterValue, enterOuts, envAfterEnter) <- enterContextManager evalExprFn envAfterCM fenv contextManager withPos
  
  -- Bind the result of __enter__ to the variable if specified
  let envAfterBind = bindContextResult maybeVarName enterValue envAfterEnter
  
  -- Execute the body of the with statement and handle exceptions
  let execBody = evalStatementsFn envAfterBind fenv [] body
  case execBody of
    Right (envAfterBody, fenvAfterBody, bodyOuts, _) -> do
      -- Exit the context manager normally
      (_, exitOuts, _) <- exitContextManager evalExprFn envAfterBody fenvAfterBody contextManager withPos
      -- Continue with the rest of the statements
      evalStatementsFn envAfterBody fenvAfterBody (outputs ++ cmOuts ++ enterOuts ++ bodyOuts ++ exitOuts) rest
    Left err -> do
      -- Exit the context manager with exception
      (exitValue, exitOuts, _) <- exitContextManagerWithException evalExprFn envAfterEnter fenv contextManager withPos err
      case exitValue of
        IntValue 0 -> 
          -- Exception not suppressed (exit returned falsy value), re-raise the original error
          Left err
        _ -> 
          -- Exception suppressed (exit returned truthy value), continue with the rest of the statements
          evalStatementsFn envAfterEnter fenv (outputs ++ cmOuts ++ enterOuts ++ exitOuts) rest