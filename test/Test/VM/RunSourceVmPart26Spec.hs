module Test.VM.RunSourceVmPart26Spec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSourceVm (vm mvp) - part 26" $ do
  describe "augmented assignment on names" $ do
    it "applies the arithmetic operators" $ do
      runSourceVm "x = 8\nx += 3\nprint x\nx -= 4\nprint x\nx *= 2\nprint x\nx //= 3\nprint x\nx %= 3\nprint x\n"
        `shouldBe` Right ["11", "7", "14", "4", "1"]

    it "applies division as a float" $ do
      runSourceVm "x = 9\nx /= 2\nprint x\n" `shouldBe` Right ["4.5"]

    it "applies the power operator" $ do
      runSourceVm "x = 2\nx **= 3\nprint x\n" `shouldBe` Right ["8"]

    it "concatenates strings" $ do
      runSourceVm "s = 'a'\ns += 'b'\nprint s\n" `shouldBe` Right ["ab"]

    it "reports a division by zero" $ do
      runSourceVm "x = 1\nx //= 0\n" `shouldBe` Left "Value error: division by zero at 2:1"

    it "reports a name error for an undefined target" $ do
      runSourceVm "x **= 2\n" `shouldBe` Left "Name error: undefined identifier x at 1:1"

  describe "augmented assignment on attributes" $ do
    it "updates an instance attribute" $ do
      runSourceVm "class C:\n  def __init__(self):\n    self.v = 2\nc = C()\nc.v += 3\nprint c.v\nc.v **= 2\nprint c.v\n"
        `shouldBe` Right ["5", "25"]

  describe "index assignment" $ do
    it "replaces a list element" $ do
      runSourceVm "xs = [1, 2, 3]\nxs[1] = 9\nprint xs\n" `shouldBe` Right ["[1, 9, 3]"]

    it "replaces a dict value" $ do
      runSourceVm "d = {'a': 1}\nd['a'] = 7\nprint d\n" `shouldBe` Right ["{a: 7}"]

    it "supports negative list indices" $ do
      runSourceVm "xs = [1, 2, 3]\nxs[-1] = 9\nprint xs\n" `shouldBe` Right ["[1, 2, 9]"]

    it "reports an out of range index" $ do
      runSourceVm "xs = [1]\nxs[3] = 9\n" `shouldBe` Left "Value error: tuple/list index out of range at 2:3"

  describe "augmented index assignment" $ do
    it "updates a list element" $ do
      runSourceVm "xs = [1, 2, 3]\nxs[0] += 4\nprint xs\n" `shouldBe` Right ["[5, 2, 3]"]

    it "updates a dict value" $ do
      runSourceVm "d = {'a': 1}\nd['a'] += 2\nprint d\n" `shouldBe` Right ["{a: 3}"]

    it "supports all arithmetic operators" $ do
      runSourceVm "xs = [10]\nxs[0] -= 3\nprint xs\nxs[0] *= 2\nprint xs\nxs[0] **= 2\nprint xs\n"
        `shouldBe` Right ["[7]", "[14]", "[196]"]

  describe "augmented assignment parity" $ do
    it "matches the expanded assignment" $ do
      runSourceVm "x = 5\nx += 3\nprint x == (5 + 3)\nxs = [2]\nxs[0] *= 4\nprint xs == [8]\n"
        `shouldBe` Right ["1", "1"]
