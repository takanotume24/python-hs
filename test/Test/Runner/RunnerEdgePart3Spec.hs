{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Runner.RunnerEdgePart3Spec (spec) where

import PythonHS.Runner (runSource)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSource (integration edge/error) - part 3" $ do
  it "runs if/else and function return in one script" $ do
    runSource "x = 0\nif x:\nprint 1\nelse:\nprint 2\ndef id(v):\nreturn v\nprint id(7)\n" `shouldBe` Right ["2", "7"]

  it "runs if/elif/else and selects elif branch" $ do
    runSource "x = 0\ny = 1\nif x:\nprint 10\nelif y:\nprint 20\nelse:\nprint 30\n" `shouldBe` Right ["20"]

  it "skips while body when condition is false" $ do
    runSource "x = 0\nwhile x < 0:\nprint x\nprint 9\n" `shouldBe` Right ["9"]

  it "executes while loop body repeatedly until condition turns false" $ do
    runSource "x = 0\nwhile x < 3:\nx = x + 1\nprint x\n" `shouldBe` Right ["3"]
