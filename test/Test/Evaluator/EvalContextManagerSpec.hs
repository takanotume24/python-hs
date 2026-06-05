module Test.Evaluator.EvalContextManagerSpec (spec) where

import PythonHS.AST.Expr (Expr (..))
import PythonHS.AST.WithContext (ContextManager(..), WithEntry(..), WithExit(..))
import PythonHS.Evaluator.Env (Env)
import PythonHS.Evaluator.EvalContextManager (bindContextResult, enterContextManager, exitContextManager, exitContextManagerWithException)
import PythonHS.Evaluator.FuncEnv (FuncEnv)
import PythonHS.Evaluator.Value (Value (IntValue, StringValue))
import PythonHS.Lexer.Position (Position (Position))
import Test.Hspec (Spec, describe, it, shouldBe)
import qualified Data.Map.Strict as Map

spec :: Spec
spec = describe "EvalContextManager" $ do
  let dummyEnv :: Env
      dummyEnv = mempty
      
      dummyFEnv :: FuncEnv
      dummyFEnv = mempty
      
      dummyPos :: Position
      dummyPos = Position 0 0
      
      dummyEvalExpr :: Env -> FuncEnv -> Expr -> Either String (Value, [String], Env)
      dummyEvalExpr _ _ _ = Right (IntValue 42, [], mempty)

  describe "enterContextManager" $ do
    it "calls __enter__ method on context manager" $ do
      let contextManagerExpr = IdentifierExpr "cm" dummyPos
          contextManager = ContextManager contextManagerExpr Nothing dummyPos
      enterContextManager dummyEvalExpr dummyEnv dummyFEnv contextManager
        `shouldBe` Right (IntValue 42, [], mempty)

    it "creates WithEntry record with correct fields" $ do
      let contextManagerExpr = IdentifierExpr "cm" dummyPos
          contextManager = ContextManager contextManagerExpr (Just "var") dummyPos
          testEvalExpr :: Env -> FuncEnv -> Expr -> Either String (Value, [String], Env)
          testEvalExpr _ _ (CallExpr "__enter__" [IdentifierExpr "cm" _] _) = Right (StringValue "entered", [], Map.singleton "entered" (StringValue "value"))
          testEvalExpr _ _ _ = Left "Unexpected expression"
      case enterContextManager testEvalExpr dummyEnv dummyFEnv contextManager of
        Right (value, outputs, env) -> do
          value `shouldBe` StringValue "entered"
        Left err -> fail err

    it "passes correct expression to evalExprFn" $ do
      let contextManagerExpr = IdentifierExpr "cm" dummyPos
          contextManager = ContextManager contextManagerExpr Nothing dummyPos
          testEvalExpr :: Env -> FuncEnv -> Expr -> Either String (Value, [String], Env)
          testEvalExpr _ _ (CallExpr "__enter__" [IdentifierExpr "cm" pos] _) = Right (StringValue "result", [], mempty)
          testEvalExpr _ _ _ = Left "Wrong expression passed to evalExprFn"
      enterContextManager testEvalExpr dummyEnv dummyFEnv contextManager
        `shouldBe` Right (StringValue "result", [], mempty)

  describe "exitContextManager" $ do
    it "calls __exit__ method with None arguments" $ do
      let contextManagerExpr = IdentifierExpr "cm" dummyPos
          contextManager = ContextManager contextManagerExpr Nothing dummyPos
      exitContextManager dummyEvalExpr dummyEnv dummyFEnv contextManager
        `shouldBe` Right (IntValue 42, [], mempty)

    it "creates WithExit record with correct fields for normal exit" $ do
      let contextManagerExpr = IdentifierExpr "cm" dummyPos
          contextManager = ContextManager contextManagerExpr Nothing dummyPos
          testEvalExpr :: Env -> FuncEnv -> Expr -> Either String (Value, [String], Env)
          testEvalExpr _ _ (CallExpr "__exit__" [IdentifierExpr "cm" _, NoneExpr _, NoneExpr _, NoneExpr _] _) = Right (IntValue 1, [], mempty)
          testEvalExpr _ _ _ = Left "Unexpected expression"
      case exitContextManager testEvalExpr dummyEnv dummyFEnv contextManager of
        Right (value, outputs, env) -> do
          value `shouldBe` IntValue 1
        Left err -> fail err

    it "passes correct expression to evalExprFn for normal exit" $ do
      let contextManagerExpr = IdentifierExpr "cm" dummyPos
          contextManager = ContextManager contextManagerExpr Nothing dummyPos
          testEvalExpr :: Env -> FuncEnv -> Expr -> Either String (Value, [String], Env)
          testEvalExpr _ _ (CallExpr "__exit__" [IdentifierExpr "cm" _, NoneExpr _, NoneExpr _, NoneExpr _] _) = Right (IntValue 0, [], mempty)
          testEvalExpr _ _ _ = Left "Wrong expression passed to evalExprFn"
      exitContextManager testEvalExpr dummyEnv dummyFEnv contextManager
        `shouldBe` Right (IntValue 0, [], mempty)

  describe "exitContextManagerWithException" $ do
    it "calls __exit__ method with exception arguments" $ do
      let contextManagerExpr = IdentifierExpr "cm" dummyPos
          contextManager = ContextManager contextManagerExpr Nothing dummyPos
          errorMessage = "Runtime error: test exception"
      exitContextManagerWithException dummyEvalExpr dummyEnv dummyFEnv contextManager errorMessage
        `shouldBe` Right (IntValue 42, [], mempty)

    it "creates WithExit record with correct fields for exception exit" $ do
      let contextManagerExpr = IdentifierExpr "cm" dummyPos
          contextManager = ContextManager contextManagerExpr Nothing dummyPos
          errorMessage = "Runtime error: test exception"
          testEvalExpr :: Env -> FuncEnv -> Expr -> Either String (Value, [String], Env)
          testEvalExpr _ _ (CallExpr "__exit__" [IdentifierExpr "cm" _, StringExpr "Exception" _, StringExpr "Runtime error: test exception" _, NoneExpr _] _) = Right (IntValue 1, [], mempty)
          testEvalExpr _ _ _ = Left "Unexpected expression"
      case exitContextManagerWithException testEvalExpr dummyEnv dummyFEnv contextManager errorMessage of
        Right (value, outputs, env) -> do
          value `shouldBe` IntValue 1
        Left err -> fail err

    it "passes correct expression to evalExprFn for exception exit" $ do
      let contextManagerExpr = IdentifierExpr "cm" dummyPos
          contextManager = ContextManager contextManagerExpr Nothing dummyPos
          errorMessage = "Runtime error: test exception"
          testEvalExpr :: Env -> FuncEnv -> Expr -> Either String (Value, [String], Env)
          testEvalExpr _ _ (CallExpr "__exit__" [IdentifierExpr "cm" _, StringExpr "Exception" _, StringExpr "Runtime error: test exception" _, NoneExpr _] _) = Right (IntValue 1, [], mempty)
          testEvalExpr _ _ _ = Left "Wrong expression passed to evalExprFn"
      exitContextManagerWithException testEvalExpr dummyEnv dummyFEnv contextManager errorMessage
        `shouldBe` Right (IntValue 1, [], mempty)

  describe "bindContextResult" $ do
    it "binds value to variable when name is provided" $ do
      let result = bindContextResult (Just "var") (StringValue "test") dummyEnv
      result `shouldBe` Map.singleton "var" (StringValue "test")

    it "returns unchanged environment when no variable name" $ do
      let result = bindContextResult Nothing (StringValue "test") dummyEnv
      result `shouldBe` dummyEnv