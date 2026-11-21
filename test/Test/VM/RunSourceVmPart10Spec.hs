module Test.VM.RunSourceVmPart10Spec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSourceVm (vm mvp) - part 10" $ do
  describe "enumerate builtin" $ do
    it "pairs list items with indices" $ do
      runSourceVm "print enumerate([10, 20])\n" `shouldBe` Right ["[(0, 10), (1, 20)]"]

    it "pairs tuple items with indices" $ do
      runSourceVm "print enumerate((10, 20))\n" `shouldBe` Right ["[(0, 10), (1, 20)]"]

    it "pairs string characters with indices" $ do
      runSourceVm "print enumerate('ab')\n" `shouldBe` Right ["[(0, a), (1, b)]"]

    it "pairs dict keys with indices" $ do
      runSourceVm "print enumerate({'a': 1, 'b': 2})\n" `shouldBe` Right ["[(0, a), (1, b)]"]

    it "starts at the given index" $ do
      runSourceVm "print enumerate([10, 20], 5)\n" `shouldBe` Right ["[(5, 10), (6, 20)]"]

    it "returns an empty list for an empty iterable" $ do
      runSourceVm "print enumerate([])\n" `shouldBe` Right ["[]"]

    it "works in a for loop" $ do
      runSourceVm "for pair in enumerate([10, 20]):\n    print pair\n" `shouldBe` Right ["(0, 10)", "(1, 20)"]

    it "reports type error for non-iterable" $ do
      runSourceVm "print enumerate(5)\n" `shouldBe` Left "Type error: enumerate expects iterable at 1:7"

    it "reports type error for non-int start" $ do
      runSourceVm "print enumerate([1], 'a')\n" `shouldBe` Left "Type error: enumerate start must be int at 1:7"

    it "reports argument count mismatch when no argument" $ do
      runSourceVm "print enumerate()\n" `shouldBe` Left "Argument count mismatch when calling enumerate at 1:7"

    it "reports argument count mismatch when too many arguments" $ do
      runSourceVm "print enumerate([1], 0, 1)\n" `shouldBe` Left "Argument count mismatch when calling enumerate at 1:7"

  describe "zip builtin" $ do
    it "zips two lists" $ do
      runSourceVm "print zip([1, 2], [3, 4])\n" `shouldBe` Right ["[(1, 3), (2, 4)]"]

    it "stops at the shortest iterable" $ do
      runSourceVm "print zip([1, 2, 3], ['a', 'b'])\n" `shouldBe` Right ["[(1, a), (2, b)]"]

    it "returns an empty list with no arguments" $ do
      runSourceVm "print zip()\n" `shouldBe` Right ["[]"]

    it "wraps a single iterable into one-element tuples" $ do
      runSourceVm "print zip([1, 2])\n" `shouldBe` Right ["[(1,), (2,)]"]

    it "zips three lists" $ do
      runSourceVm "print zip([1], [2], [3])\n" `shouldBe` Right ["[(1, 2, 3)]"]

    it "zips a string and a list" $ do
      runSourceVm "print zip('ab', [1, 2])\n" `shouldBe` Right ["[(a, 1), (b, 2)]"]

    it "zips dict keys and a list" $ do
      runSourceVm "print zip({'a': 1}, [9])\n" `shouldBe` Right ["[(a, 9)]"]

    it "reports type error when a later argument is not iterable" $ do
      runSourceVm "print zip([1], 5)\n" `shouldBe` Left "Type error: zip expects iterable at 1:7"

    it "reports type error for a non-iterable first argument" $ do
      runSourceVm "print zip(5)\n" `shouldBe` Left "Type error: zip expects iterable at 1:7"
