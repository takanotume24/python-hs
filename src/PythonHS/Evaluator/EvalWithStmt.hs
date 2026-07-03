module PythonHS.Evaluator.EvalWithStmt (evalWithStmt) where

import PythonHS.AST.Expr (Expr)
import PythonHS.AST.Stmt (Stmt)
import PythonHS.AST.WithContext (ContextManager(..))
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalContextManager (bindContextResult, enterContextManager, exitContextManager, exitContextManagerWithException)
import PythonHS.Evaluator.EvalWithStmtConfig (EvalWithStmtConfig (..))
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Evaluator.EvalExprResult (EvalExprResult (..))
import PythonHS.Evaluator.Value (Value (IntValue))
import PythonHS.Lexer.Position (Position)

evalWithStmt ::
  EvalWithStmtConfig ->
  Env ->
  FuncEnv ->
  [String] ->
  Expr ->
  Maybe String ->
  [Stmt] ->
  Position ->
  [Stmt] ->
  Either String (Env, FuncEnv, [String], Maybe (Value, Position))
evalWithStmt config env fenv outputs contextManager maybeVarName body withPos rest = do
  let evalStatementsFn = evalWithStmtEvalStatements config
      evalExprFn = evalWithStmtEvalExpr config
  -- Create context manager record
  let ctxManager = ContextManager contextManager maybeVarName withPos
  
  -- Evaluate the context manager expression
  cmResult <- evalExprFn env fenv (contextManagerExpr ctxManager)
  let cmOuts = evalExprResultOutputs cmResult
      envAfterCM = evalExprResultEnv cmResult
  
  -- Enter the context manager using the record
  enterResult <- enterContextManager evalExprFn envAfterCM fenv ctxManager
  let enterValue = evalExprResultValue enterResult
      enterOuts = evalExprResultOutputs enterResult
      envAfterEnter = evalExprResultEnv enterResult
  
  -- Bind the result of __enter__ to the variable if specified
  let envAfterBind = bindContextResult (contextManagerVarName ctxManager) enterValue envAfterEnter
  
  -- Execute the body of the with statement and handle exceptions
  let execBody = evalStatementsFn envAfterBind fenv [] body
  case execBody of
    Right (envAfterBody, fenvAfterBody, bodyOuts, _) -> do
      -- Exit the context manager normally
      exitResult <- exitContextManager evalExprFn envAfterBody fenvAfterBody ctxManager
      let exitOuts = evalExprResultOutputs exitResult
      -- Continue with the rest of the statements
      evalStatementsFn envAfterBody fenvAfterBody (outputs ++ cmOuts ++ enterOuts ++ bodyOuts ++ exitOuts) rest
    Left err -> do
      -- Exit the context manager with exception
      exitResult <- exitContextManagerWithException evalExprFn envAfterEnter fenv ctxManager err
      let exitValue = evalExprResultValue exitResult
          exitOuts = evalExprResultOutputs exitResult
      case exitValue of
        IntValue 0 -> 
          -- Exception not suppressed (exit returned falsy value), re-raise the original error
          Left err
        _ -> 
          -- Exception suppressed (exit returned truthy value), continue with the rest of the statements
          evalStatementsFn envAfterEnter fenv (outputs ++ cmOuts ++ enterOuts ++ exitOuts) rest
