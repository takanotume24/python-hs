module PythonHS.Evaluator.EvalContextManager (enterContextManager, exitContextManager, exitContextManagerWithException, bindContextResult) where

import qualified Data.Map.Strict as Map
import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.WithContext (ContextManager(..), WithEntry(..), WithExit(..))
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Evaluator.Value (Value)
import PythonHS.Lexer.Position (Position)
import PythonHS.VM.Instruction (Instruction(CallFunction))

-- | Enter a context manager by calling its __enter__ method
--
-- This function calls the __enter__ method on the context manager and returns
-- the result of that call. If the context manager does not have an __enter__
-- method or if calling it results in an error, the error is propagated up.
--
-- The context manager should implement the context manager protocol by defining
-- both __enter__ and __exit__ methods.
--
-- >>> enterContextManager evalExprFn env fenv (IdentifierExpr "cm" pos) pos
-- Right (resultValue, outputs, newEnv)
--
-- Example usage in a with statement:
-- >>> -- For "with open('file.txt') as f:"
-- >>> -- This would call open('file.txt').__enter__() and bind the result to 'f'
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

-- | Exit a context manager normally by calling its __exit__ method with None arguments
--
-- This function calls the __exit__ method on the context manager with None arguments
-- for exc_type, exc_value, and traceback, indicating normal exit (no exception).
--
-- The __exit__ method should return a truthy value to suppress exceptions or
-- a falsy value (including None) to allow exceptions to propagate.
--
-- >>> exitContextManager evalExprFn env fenv (IdentifierExpr "cm" pos) pos
-- Right (exitValue, outputs, newEnv)
--
-- Example usage in a with statement:
-- >>> -- For normal exit from "with open('file.txt') as f:" 
-- >>> -- This would call f.__exit__(None, None, None)
exitContextManager ::
  (Env -> FuncEnv -> Expr -> Either String (Value, [String], Env)) ->
  Env ->
  FuncEnv ->
  ContextManager ->
  Either String (Value, [String], Env)
exitContextManager evalExprFn env fenv contextManager = do
  let noneExpr = NoneExpr (contextManagerPos contextManager)
  let exitCall = CallExpr "__exit__" [contextManagerExpr contextManager, noneExpr, noneExpr, noneExpr] (contextManagerPos contextManager)
  let exitInstruction = CallFunction "__exit__" [] (contextManagerPos contextManager)
  let exitNormal = WithExit exitCall exitInstruction (contextManagerPos contextManager) False
  evalExprFn env fenv (exitCallExpr exitNormal)

-- | Exit a context manager with an exception by calling its __exit__ method with exception arguments
--
-- This function calls the __exit__ method on the context manager with exception information,
-- allowing the context manager to handle or suppress the exception.
--
-- The exception arguments are:
-- 1. exc_type: "Exception" (as a string)
-- 2. exc_value: The error message
-- 3. traceback: None (simplified representation)
--
-- The __exit__ method should return a truthy value to suppress the exception or
-- a falsy value (including None) to allow the exception to propagate.
--
-- >>> exitContextManagerWithException evalExprFn env fenv (IdentifierExpr "cm" pos) pos "Runtime error: test"
-- Right (exitValue, outputs, newEnv)
--
-- Example usage in a with statement:
-- >>> -- For exception exit from "with open('file.txt') as f:" 
-- >>> -- This would call f.__exit__("Exception", "Runtime error: ...", None)
exitContextManagerWithException ::
  (Env -> FuncEnv -> Expr -> Either String (Value, [String], Env)) ->
  Env ->
  FuncEnv ->
  ContextManager ->
  String ->
  Either String (Value, [String], Env)
exitContextManagerWithException evalExprFn env fenv contextManager err = do
  let withPos = contextManagerPos contextManager
  let exitCall = CallExpr "__exit__" [contextManagerExpr contextManager, StringExpr "Exception" withPos, StringExpr err withPos, NoneExpr withPos] withPos
  let exitInstruction = CallFunction "__exit__" [] withPos
  let exitException = WithExit exitCall exitInstruction withPos True
  evalExprFn env fenv (exitCallExpr exitException)

-- | Bind the result of context manager's __enter__ method to a variable if specified
--
-- This function binds the result value from the context manager's __enter__ method
-- to the specified variable name in the environment. If no variable name is specified,
-- the environment is returned unchanged.
--
-- In Python with statements, this corresponds to the "as variable" part:
-- "with expr as variable:" binds the result of expr.__enter__() to 'variable'
--
-- >>> bindContextResult (Just "f") (StringValue "file_handle") env
-- envWithFBound
--
-- >>> bindContextResult Nothing (StringValue "file_handle") env
-- env  -- unchanged
--
-- Example usage:
-- >>> -- For "with open('file.txt') as f:"
-- >>> -- bindContextResult (Just "f") fileObject env
-- >>> -- Would bind the file object to variable 'f' in the environment
bindContextResult :: Maybe String -> Value -> Env -> Env
bindContextResult maybeVarName enterValue env =
  case maybeVarName of
    Just varName -> Map.insert varName enterValue env
    Nothing -> env