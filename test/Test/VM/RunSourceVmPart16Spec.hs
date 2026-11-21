module Test.VM.RunSourceVmPart16Spec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSourceVm (vm mvp) - part 16" $ do
  describe "list index builtin" $ do
    it "returns the first index of a value" $ do
      runSourceVm "print ['a', 'b', 'c'].index('b')\n" `shouldBe` Right ["1"]

    it "returns zero for a value at the head" $ do
      runSourceVm "print index([1, 2, 3], 1)\n" `shouldBe` Right ["0"]

    it "returns the first occurrence for duplicate values" $ do
      runSourceVm "print index([1, 2, 1], 1)\n" `shouldBe` Right ["0"]

    it "reports value error when the value is absent" $ do
      runSourceVm "print index([1, 2], 5)\n" `shouldBe` Left "Value error: index value not found at 1:7"

    it "reports type error for an invalid receiver" $ do
      runSourceVm "print index(5, 'a')\n" `shouldBe` Left "Type error: index expects list as first argument at 1:7"

    it "reports argument count mismatch" $ do
      runSourceVm "print index([1])\n" `shouldBe` Left "Argument count mismatch when calling index at 1:7"

  describe "list count builtin" $ do
    it "counts matching values" $ do
      runSourceVm "print [1, 2, 1].count(1)\n" `shouldBe` Right ["2"]

    it "returns zero when the value is absent" $ do
      runSourceVm "print count([1, 2], 3)\n" `shouldBe` Right ["0"]

    it "counts matching strings" $ do
      runSourceVm "print count(['a', 'a', 'b'], 'a')\n" `shouldBe` Right ["2"]

    it "returns zero for an empty list" $ do
      runSourceVm "print count([], 1)\n" `shouldBe` Right ["0"]

    it "keeps string count working" $ do
      runSourceVm "print 'banana'.count('a')\n" `shouldBe` Right ["3"]

    it "reports argument count mismatch" $ do
      runSourceVm "print count([1])\n" `shouldBe` Left "Argument count mismatch when calling count at 1:7"

  describe "list copy builtin" $ do
    it "copies a list" $ do
      runSourceVm "xs = [1, 2]\nprint xs.copy()\n" `shouldBe` Right ["[1, 2]"]

    it "copies an empty list" $ do
      runSourceVm "print copy([])\n" `shouldBe` Right ["[]"]

    it "supports method syntax on a list literal" $ do
      runSourceVm "print [1, 2].copy()\n" `shouldBe` Right ["[1, 2]"]

    it "reports type error for a non-collection" $ do
      runSourceVm "print copy(5)\n" `shouldBe` Left "Type error: copy expects list or dict at 1:7"

    it "reports argument count mismatch" $ do
      runSourceVm "print copy()\n" `shouldBe` Left "Argument count mismatch when calling copy at 1:7"

  describe "dict copy builtin" $ do
    it "copies a dict" $ do
      runSourceVm "d = {'a': 1}\nprint d.copy()\n" `shouldBe` Right ["{a: 1}"]

    it "copies an empty dict" $ do
      runSourceVm "print copy({})\n" `shouldBe` Right ["{}"]

    it "preserves insertion order" $ do
      runSourceVm "print copy({'a': 1, 'b': 2})\n" `shouldBe` Right ["{a: 1, b: 2}"]

  describe "fromkeys builtin" $ do
    it "builds a dict from keys with a default value" $ do
      runSourceVm "print fromkeys(['a', 'b'], 0)\n" `shouldBe` Right ["{a: 0, b: 0}"]

    it "deduplicates keys" $ do
      runSourceVm "print fromkeys(['a', 'a', 'b'], 1)\n" `shouldBe` Right ["{a: 1, b: 1}"]

    it "defaults values to None" $ do
      runSourceVm "print fromkeys([1, 2])\n" `shouldBe` Right ["{1: None, 2: None}"]

    it "accepts a string as the key iterable" $ do
      runSourceVm "print fromkeys('ab', 0)\n" `shouldBe` Right ["{a: 0, b: 0}"]

    it "reports type error for a non-iterable" $ do
      runSourceVm "print fromkeys(5)\n" `shouldBe` Left "Type error: fromkeys expects iterable at 1:7"

    it "reports argument count mismatch" $ do
      runSourceVm "print fromkeys()\n" `shouldBe` Left "Argument count mismatch when calling fromkeys at 1:7"

  describe "popitem builtin" $ do
    it "returns the last inserted pair" $ do
      runSourceVm "d = {'a': 1, 'b': 2}\nprint d.popitem()\n" `shouldBe` Right ["(b, 2)"]

    it "returns the only pair of a single-entry dict" $ do
      runSourceVm "print popitem({'a': 1})\n" `shouldBe` Right ["(a, 1)"]

    it "reports value error for an empty dict" $ do
      runSourceVm "print popitem({})\n" `shouldBe` Left "Value error: popitem from empty dict at 1:7"

    it "reports type error for a non-dict" $ do
      runSourceVm "print popitem([])\n" `shouldBe` Left "Type error: popitem expects dict at 1:7"

    it "reports argument count mismatch" $ do
      runSourceVm "print popitem()\n" `shouldBe` Left "Argument count mismatch when calling popitem at 1:7"
