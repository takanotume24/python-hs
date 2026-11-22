module Test.VM.RunSourceVmPart27Spec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSourceVm (vm mvp) - part 27" $ do
  describe "slice assignment" $ do
    it "replaces a middle slice" $ do
      runSourceVm "xs = [0, 1, 2, 3, 4]\nxs[1:3] = [9, 8]\nprint xs\n" `shouldBe` Right ["[0, 9, 8, 3, 4]"]

    it "supports an omitted start" $ do
      runSourceVm "xs = [0, 1, 2, 3]\nxs[:2] = [7]\nprint xs\n" `shouldBe` Right ["[7, 2, 3]"]

    it "supports an omitted end" $ do
      runSourceVm "xs = [0, 1, 2, 3]\nxs[1:] = [7, 8]\nprint xs\n" `shouldBe` Right ["[0, 7, 8]"]

    it "supports negative bounds" $ do
      runSourceVm "xs = [0, 1, 2, 3, 4]\nxs[-2:] = [9]\nprint xs\n" `shouldBe` Right ["[0, 1, 2, 9]"]

    it "inserts into an empty slice" $ do
      runSourceVm "xs = [0, 1, 2, 3]\nxs[2:2] = [9]\nprint xs\n" `shouldBe` Right ["[0, 1, 9, 2, 3]"]

    it "inserts when start is greater than end" $ do
      runSourceVm "xs = [0, 1, 2, 3]\nxs[3:1] = [9]\nprint xs\n" `shouldBe` Right ["[0, 1, 2, 9, 3]"]

    it "accepts a tuple as the replacement" $ do
      runSourceVm "xs = [0, 1, 2, 3]\nxs[1:2] = (7, 8)\nprint xs\n" `shouldBe` Right ["[0, 7, 8, 2, 3]"]

    it "reports a non-iterable replacement" $ do
      runSourceVm "xs = [1, 2, 3]\nxs[1:2] = 5\n" `shouldBe` Left "Type error: can only assign an iterable to a slice at 2:3"

    it "keeps the value when assigning an existing slice back" $ do
      runSourceVm "xs = [0, 1, 2, 3]\nys = xs[1:3]\nxs[1:3] = ys\nprint xs\n" `shouldBe` Right ["[0, 1, 2, 3]"]

  describe "del on list elements" $ do
    it "deletes an index" $ do
      runSourceVm "xs = [1, 2, 3]\ndel xs[1]\nprint xs\n" `shouldBe` Right ["[1, 3]"]

    it "deletes a negative index" $ do
      runSourceVm "xs = [1, 2, 3]\ndel xs[-1]\nprint xs\n" `shouldBe` Right ["[1, 2]"]

    it "reports an out of range index" $ do
      runSourceVm "xs = [1]\ndel xs[5]\n" `shouldBe` Left "Value error: tuple/list index out of range at 2:7"

  describe "del on dict entries" $ do
    it "deletes a key" $ do
      runSourceVm "d = {'a': 1, 'b': 2}\ndel d['a']\nprint d\n" `shouldBe` Right ["{b: 2}"]

    it "reports a missing key" $ do
      runSourceVm "d = {'a': 1}\ndel d['z']\n" `shouldBe` Left "Key not found in delitem at 2:6"

  describe "del on slices" $ do
    it "deletes a middle slice" $ do
      runSourceVm "xs = [1, 2, 3, 4]\ndel xs[1:3]\nprint xs\n" `shouldBe` Right ["[1, 4]"]

    it "supports an omitted start" $ do
      runSourceVm "xs = [1, 2, 3, 4]\ndel xs[:2]\nprint xs\n" `shouldBe` Right ["[3, 4]"]

    it "supports negative bounds" $ do
      runSourceVm "xs = [1, 2, 3, 4]\ndel xs[-2:]\nprint xs\n" `shouldBe` Right ["[1, 2]"]

    it "deletes an empty slice without changing the list" $ do
      runSourceVm "xs = [1, 2, 3]\ndel xs[1:1]\nprint xs\n" `shouldBe` Right ["[1, 2, 3]"]
