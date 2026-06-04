module PythonHS.Evaluator.EvalWithStmt (evalWithStmt) where

import qualified Data.Map.Strict as Map
import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.Stmt (Stmt)
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Evaluator.ShowPos (showPos)
import PythonHS.Evaluator.Value (Value)
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
  (cmValue, cmOuts, envAfterCM) <- evalExprFn env fenv contextManager
  
  -- For simplicity, we'll assume the context manager is an object with __enter__ and __exit__ methods
  -- In a more complete implementation, we would need to properly handle method lookup
  
  -- Create a call to the __enter__ method
  let enterCall = CallExpr "__enter__" [contextManager] withPos
  (enterValue, enterOuts, envAfterEnter) <- evalExprFn envAfterCM fenv enterCall
  
  -- Bind the result of __enter__ to the variable if specified
  let envAfterBind = case maybeVarName of
        Just varName -> Map.insert varName enterValue envAfterEnter
        Nothing -> envAfterEnter
  
  -- Execute the body of the with statement
  (envAfterBody, fenvAfterBody, bodyOuts, bodyRet) <- evalStatementsFn envAfterBind fenv [] body
  
  -- Call __exit__ method on the context manager
  let exitCall = CallExpr "__exit__" [contextManager, NoneExpr withPos, NoneExpr withPos, NoneExpr withPos] withPos
  (_exitValue, exitOuts, _envAfterExit) <- evalExprFn envAfterBody fenvAfterBody exitCall
  
  -- Continue with the rest of the statements
  evalStatementsFn envAfterBody fenvAfterBody (outputs ++ cmOuts ++ enterOuts ++ bodyOuts ++ exitOuts) rest