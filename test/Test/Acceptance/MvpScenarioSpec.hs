module Test.Acceptance.MvpScenarioSpec (spec) where

import PythonHS.Runner (runSource)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "MVP acceptance scenario" $ do
  it "runs assignment, if, while, print, and function return in one script" $ do
    runSource
      "x = 1\nif x:\nprint 10\nelse:\nprint 20\nwhile x < 3:\nx = x + 1\ndef id(v):\nreturn v\nprint id(x)\n"
      `shouldBe` Right ["10", "3"]
