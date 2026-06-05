module Test.Evaluator.EvalContextManagerSpec (spec) where

import PythonHS.AST.Expr (Expr (..))
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
      let contextManager = IdentifierExpr "cm" dummyPos
      enterContextManager dummyEvalExpr dummyEnv dummyFEnv contextManager dummyPos
        `shouldBe` Right (IntValue 42, [], mempty)

  describe "exitContextManager" $ do
    it "calls __exit__ method with None arguments" $ do
      let contextManager = IdentifierExpr "cm" dummyPos
      exitContextManager dummyEvalExpr dummyEnv dummyFEnv contextManager dummyPos
        `shouldBe` Right (IntValue 42, [], mempty)

  describe "exitContextManagerWithException" $ do
    it "calls __exit__ method with exception arguments" $ do
      let contextManager = IdentifierExpr "cm" dummyPos
          errorMessage = "Runtime error: test exception"
      exitContextManagerWithException dummyEvalExpr dummyEnv dummyFEnv contextManager dummyPos errorMessage
        `shouldBe` Right (IntValue 42, [], mempty)

  describe "bindContextResult" $ do
    it "binds value to variable when name is provided" $ do
      let result = bindContextResult (Just "var") (StringValue "test") dummyEnv
      result `shouldBe` Map.singleton "var" (StringValue "test")

    it "returns unchanged environment when no variable name" $ do
      let result = bindContextResult Nothing (StringValue "test") dummyEnv
      result `shouldBe` dummyEnv