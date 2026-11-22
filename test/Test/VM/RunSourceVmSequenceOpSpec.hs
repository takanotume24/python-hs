module Test.VM.RunSourceVmSequenceOpSpec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSourceVm sequence operators" $ do
  it "concatenates lists" $ do
    runSourceVm "print([1, 2] + [3])\n" `shouldBe` Right ["[1, 2, 3]"]

  it "concatenates tuples" $ do
    runSourceVm "print((1, 2) + (3,))\n" `shouldBe` Right ["(1, 2, 3)"]

  it "repeats strings" $ do
    runSourceVm "print(\"ab\" * 3)\n" `shouldBe` Right ["ababab"]

  it "repeats strings from the left" $ do
    runSourceVm "print(3 * \"ab\")\n" `shouldBe` Right ["ababab"]

  it "repeats lists" $ do
    runSourceVm "print([0] * 3)\n" `shouldBe` Right ["[0, 0, 0]"]

  it "concatenates lists through augmented assignment" $ do
    runSourceVm "x = [1]\nx += [2, 3]\nprint(x)\n" `shouldBe` Right ["[1, 2, 3]"]

  it "repeats lists through augmented assignment" $ do
    runSourceVm "x = [1, 2]\nx *= 2\nprint(x)\n" `shouldBe` Right ["[1, 2, 1, 2]"]
