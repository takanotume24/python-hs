module Test.VM.RunSourceVmAugAssignBitwiseSpec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSourceVm bitwise augmented assignment" $ do
  it "supports augmented assignment on names" $ do
    runSourceVm "x = 6\nx &= 3\nx |= 8\nx ^= 1\nx <<= 2\nx >>= 1\nprint(x)\n"
      `shouldBe` Right ["22"]

  it "supports augmented assignment on subscripts" $ do
    runSourceVm "x = [6]\nx[0] &= 3\nprint(x[0])\n"
      `shouldBe` Right ["2"]

  it "supports augmented assignment on attributes" $ do
    runSourceVm "class A:\n    x = 6\nA.x &= 3\nprint(A.x)\n"
      `shouldBe` Right ["2"]
