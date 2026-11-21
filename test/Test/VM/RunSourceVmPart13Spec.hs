module Test.VM.RunSourceVmPart13Spec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSourceVm (vm mvp) - part 13" $ do
  describe "any builtin" $ do
    it "returns 1 when at least one element is truthy" $ do
      runSourceVm "print any([0, 1])\n" `shouldBe` Right ["1"]

    it "returns 0 when every element is falsy" $ do
      runSourceVm "print any([0, 0])\n" `shouldBe` Right ["0"]

    it "returns 0 for an empty list" $ do
      runSourceVm "print any([])\n" `shouldBe` Right ["0"]

    it "treats empty strings as falsy" $ do
      runSourceVm "print any([0, ''])\n" `shouldBe` Right ["0"]

    it "accepts a tuple" $ do
      runSourceVm "print any((0, 2))\n" `shouldBe` Right ["1"]

    it "accepts dict keys" $ do
      runSourceVm "print any({'a': 0})\n" `shouldBe` Right ["1"]

    it "returns 0 for an empty string" $ do
      runSourceVm "print any('')\n" `shouldBe` Right ["0"]

    it "reports type error for non-iterable" $ do
      runSourceVm "print any(5)\n" `shouldBe` Left "Type error: any expects iterable at 1:7"

    it "reports argument count mismatch" $ do
      runSourceVm "print any()\n" `shouldBe` Left "Argument count mismatch when calling any at 1:7"

  describe "all builtin" $ do
    it "returns 1 when every element is truthy" $ do
      runSourceVm "print all([1, 2])\n" `shouldBe` Right ["1"]

    it "returns 0 when at least one element is falsy" $ do
      runSourceVm "print all([1, 0])\n" `shouldBe` Right ["0"]

    it "returns 1 for an empty list" $ do
      runSourceVm "print all([])\n" `shouldBe` Right ["1"]

    it "treats None as falsy" $ do
      runSourceVm "print all([1, None])\n" `shouldBe` Right ["0"]

    it "treats empty strings as falsy" $ do
      runSourceVm "print all([1, ''])\n" `shouldBe` Right ["0"]

    it "accepts a tuple" $ do
      runSourceVm "print all((1, 2))\n" `shouldBe` Right ["1"]

    it "accepts dict keys" $ do
      runSourceVm "print all({'a': 1})\n" `shouldBe` Right ["1"]

    it "reports type error for non-iterable" $ do
      runSourceVm "print all(5)\n" `shouldBe` Left "Type error: all expects iterable at 1:7"

    it "reports argument count mismatch" $ do
      runSourceVm "print all([1], [2])\n" `shouldBe` Left "Argument count mismatch when calling all at 1:7"
