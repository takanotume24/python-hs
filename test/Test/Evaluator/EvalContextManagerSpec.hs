module Test.Evaluator.EvalContextManagerSpec (spec) where

import Data.Map.Strict qualified as Map
import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.WithContext (ContextManager (..), WithEntry (..), WithExit (..))
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalContextManager (EnterContextManagerInput (..), ExitContextManagerInput (..), ExitContextManagerWithExceptionInput (..), bindContextResult, enterContextManager, exitContextManager, exitContextManagerWithException)
import PythonHS.Evaluator.EvalExprResult (EvalExprResult (..))
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Evaluator.Value (Value (IntValue, StringValue))
import PythonHS.Lexer.Position (Position (Position))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "EvalContextManager" $ do
  let dummyEnv :: Env
      dummyEnv = mempty

      dummyFEnv :: FuncEnv
      dummyFEnv = mempty

      dummyPos :: Position
      dummyPos = Position 0 0

      dummyEvalExpr :: Env -> FuncEnv -> Expr -> Either String EvalExprResult
      dummyEvalExpr _ _ _ = Right (EvalExprResult (IntValue 42) [] mempty)

  describe "enterContextManager" $ do
    it "calls __enter__ method on context manager" $ do
      let contextManagerExpr = IdentifierExpr "cm" dummyPos
          contextManager = ContextManager contextManagerExpr Nothing dummyPos
      enterContextManager EnterContextManagerInput {enterContextManagerEvalExprFn = dummyEvalExpr, enterContextManagerEnv = dummyEnv, enterContextManagerFuncEnv = dummyFEnv, enterContextManagerContextManager = contextManager}
        `shouldBe` Right (EvalExprResult (IntValue 42) [] mempty)

    it "creates WithEntry record with correct fields" $ do
      let contextManagerExpr = IdentifierExpr "cm" dummyPos
          contextManager = ContextManager contextManagerExpr (Just "var") dummyPos
          testEvalExpr :: Env -> FuncEnv -> Expr -> Either String EvalExprResult
          testEvalExpr _ _ (CallExpr "__enter__" [IdentifierExpr "cm" _] _) = Right (EvalExprResult (StringValue "entered") [] (Map.singleton "entered" (StringValue "value")))
          testEvalExpr _ _ _ = Left "Unexpected expression"
      case enterContextManager EnterContextManagerInput {enterContextManagerEvalExprFn = testEvalExpr, enterContextManagerEnv = dummyEnv, enterContextManagerFuncEnv = dummyFEnv, enterContextManagerContextManager = contextManager} of
        Right (EvalExprResult value outputs env) -> do
          value `shouldBe` StringValue "entered"
        Left err -> fail err

    it "passes correct expression to evalExprFn" $ do
      let contextManagerExpr = IdentifierExpr "cm" dummyPos
          contextManager = ContextManager contextManagerExpr Nothing dummyPos
          testEvalExpr :: Env -> FuncEnv -> Expr -> Either String EvalExprResult
          testEvalExpr _ _ (CallExpr "__enter__" [IdentifierExpr "cm" pos] _) = Right (EvalExprResult (StringValue "result") [] mempty)
          testEvalExpr _ _ _ = Left "Wrong expression passed to evalExprFn"
      enterContextManager EnterContextManagerInput {enterContextManagerEvalExprFn = testEvalExpr, enterContextManagerEnv = dummyEnv, enterContextManagerFuncEnv = dummyFEnv, enterContextManagerContextManager = contextManager}
        `shouldBe` Right (EvalExprResult (StringValue "result") [] mempty)

  describe "exitContextManager" $ do
    it "calls __exit__ method with None arguments" $ do
      let contextManagerExpr = IdentifierExpr "cm" dummyPos
          contextManager = ContextManager contextManagerExpr Nothing dummyPos
      exitContextManager ExitContextManagerInput {exitContextManagerEvalExprFn = dummyEvalExpr, exitContextManagerEnv = dummyEnv, exitContextManagerFuncEnv = dummyFEnv, exitContextManagerContextManager = contextManager}
        `shouldBe` Right (EvalExprResult (IntValue 42) [] mempty)

    it "creates WithExit record with correct fields for normal exit" $ do
      let contextManagerExpr = IdentifierExpr "cm" dummyPos
          contextManager = ContextManager contextManagerExpr Nothing dummyPos
          testEvalExpr :: Env -> FuncEnv -> Expr -> Either String EvalExprResult
          testEvalExpr _ _ (CallExpr "__exit__" [IdentifierExpr "cm" _, NoneExpr _, NoneExpr _, NoneExpr _] _) = Right (EvalExprResult (IntValue 1) [] mempty)
          testEvalExpr _ _ _ = Left "Unexpected expression"
      case exitContextManager ExitContextManagerInput {exitContextManagerEvalExprFn = testEvalExpr, exitContextManagerEnv = dummyEnv, exitContextManagerFuncEnv = dummyFEnv, exitContextManagerContextManager = contextManager} of
        Right (EvalExprResult value outputs env) -> do
          value `shouldBe` IntValue 1
        Left err -> fail err

    it "passes correct expression to evalExprFn for normal exit" $ do
      let contextManagerExpr = IdentifierExpr "cm" dummyPos
          contextManager = ContextManager contextManagerExpr Nothing dummyPos
          testEvalExpr :: Env -> FuncEnv -> Expr -> Either String EvalExprResult
          testEvalExpr _ _ (CallExpr "__exit__" [IdentifierExpr "cm" _, NoneExpr _, NoneExpr _, NoneExpr _] _) = Right (EvalExprResult (IntValue 0) [] mempty)
          testEvalExpr _ _ _ = Left "Wrong expression passed to evalExprFn"
      exitContextManager ExitContextManagerInput {exitContextManagerEvalExprFn = testEvalExpr, exitContextManagerEnv = dummyEnv, exitContextManagerFuncEnv = dummyFEnv, exitContextManagerContextManager = contextManager}
        `shouldBe` Right (EvalExprResult (IntValue 0) [] mempty)

  describe "exitContextManagerWithException" $ do
    it "calls __exit__ method with exception arguments" $ do
      let contextManagerExpr = IdentifierExpr "cm" dummyPos
          contextManager = ContextManager contextManagerExpr Nothing dummyPos
          errorMessage = "Runtime error: test exception"
      exitContextManagerWithException ExitContextManagerWithExceptionInput {exitContextManagerWithExceptionEvalExprFn = dummyEvalExpr, exitContextManagerWithExceptionEnv = dummyEnv, exitContextManagerWithExceptionFuncEnv = dummyFEnv, exitContextManagerWithExceptionContextManager = contextManager, exitContextManagerWithExceptionErr = errorMessage}
        `shouldBe` Right (EvalExprResult (IntValue 42) [] mempty)

    it "creates WithExit record with correct fields for exception exit" $ do
      let contextManagerExpr = IdentifierExpr "cm" dummyPos
          contextManager = ContextManager contextManagerExpr Nothing dummyPos
          errorMessage = "Runtime error: test exception"
          testEvalExpr :: Env -> FuncEnv -> Expr -> Either String EvalExprResult
          testEvalExpr _ _ (CallExpr "__exit__" [IdentifierExpr "cm" _, StringExpr "Exception" _, StringExpr "Runtime error: test exception" _, NoneExpr _] _) = Right (EvalExprResult (IntValue 1) [] mempty)
          testEvalExpr _ _ _ = Left "Unexpected expression"
      case exitContextManagerWithException ExitContextManagerWithExceptionInput {exitContextManagerWithExceptionEvalExprFn = testEvalExpr, exitContextManagerWithExceptionEnv = dummyEnv, exitContextManagerWithExceptionFuncEnv = dummyFEnv, exitContextManagerWithExceptionContextManager = contextManager, exitContextManagerWithExceptionErr = errorMessage} of
        Right (EvalExprResult value outputs env) -> do
          value `shouldBe` IntValue 1
        Left err -> fail err

    it "passes correct expression to evalExprFn for exception exit" $ do
      let contextManagerExpr = IdentifierExpr "cm" dummyPos
          contextManager = ContextManager contextManagerExpr Nothing dummyPos
          errorMessage = "Runtime error: test exception"
          testEvalExpr :: Env -> FuncEnv -> Expr -> Either String EvalExprResult
          testEvalExpr _ _ (CallExpr "__exit__" [IdentifierExpr "cm" _, StringExpr "Exception" _, StringExpr "Runtime error: test exception" _, NoneExpr _] _) = Right (EvalExprResult (IntValue 1) [] mempty)
          testEvalExpr _ _ _ = Left "Wrong expression passed to evalExprFn"
      exitContextManagerWithException ExitContextManagerWithExceptionInput {exitContextManagerWithExceptionEvalExprFn = testEvalExpr, exitContextManagerWithExceptionEnv = dummyEnv, exitContextManagerWithExceptionFuncEnv = dummyFEnv, exitContextManagerWithExceptionContextManager = contextManager, exitContextManagerWithExceptionErr = errorMessage}
        `shouldBe` Right (EvalExprResult (IntValue 1) [] mempty)

  describe "bindContextResult" $ do
    it "binds value to variable when name is provided" $ do
      let result = bindContextResult (Just "var") (StringValue "test") dummyEnv
      result `shouldBe` Map.singleton "var" (StringValue "test")

    it "returns unchanged environment when no variable name" $ do
      let result = bindContextResult Nothing (StringValue "test") dummyEnv
      result `shouldBe` dummyEnv
