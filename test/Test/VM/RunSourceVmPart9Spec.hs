module Test.VM.RunSourceVmPart9Spec (spec) where

import PythonHS.RunSourceVm (runSourceVm)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "runSourceVm (vm mvp) - part 9" $ do
  describe "list builtin" $ do
    it "creates an empty list" $ do
      runSourceVm "print list()\n" `shouldBe` Right ["[]"]

    it "copies a list" $ do
      runSourceVm "print list([1, 2])\n" `shouldBe` Right ["[1, 2]"]

    it "converts a tuple to a list" $ do
      runSourceVm "print list((1, 2))\n" `shouldBe` Right ["[1, 2]"]

    it "converts a string to a list of characters" $ do
      runSourceVm "print list('abc')\n" `shouldBe` Right ["[a, b, c]"]

    it "converts a dict to a list of keys" $ do
      runSourceVm "print list({'a': 1, 'b': 2})\n" `shouldBe` Right ["[a, b]"]

    it "converts a range result to a list" $ do
      runSourceVm "print list(range(3))\n" `shouldBe` Right ["[0, 1, 2]"]

    it "reports type error for non-iterable" $ do
      runSourceVm "print list(5)\n" `shouldBe` Left "Type error: list expects iterable at 1:7"

    it "reports argument count mismatch" $ do
      runSourceVm "print list([1], [2])\n" `shouldBe` Left "Argument count mismatch when calling list at 1:7"

  describe "tuple builtin" $ do
    it "creates an empty tuple" $ do
      runSourceVm "print tuple()\n" `shouldBe` Right ["()"]

    it "converts a list to a tuple" $ do
      runSourceVm "print tuple([1, 2])\n" `shouldBe` Right ["(1, 2)"]

    it "copies a tuple" $ do
      runSourceVm "print tuple((1, 2))\n" `shouldBe` Right ["(1, 2)"]

    it "converts a string to a tuple of characters" $ do
      runSourceVm "print tuple('ab')\n" `shouldBe` Right ["(a, b)"]

    it "renders a single element tuple with a trailing comma" $ do
      runSourceVm "print tuple({'a': 1})\n" `shouldBe` Right ["(a,)"]

    it "reports type error for non-iterable" $ do
      runSourceVm "print tuple(5)\n" `shouldBe` Left "Type error: tuple expects iterable at 1:7"

    it "reports argument count mismatch" $ do
      runSourceVm "print tuple([1], [2])\n" `shouldBe` Left "Argument count mismatch when calling tuple at 1:7"

  describe "dict builtin" $ do
    it "creates an empty dict" $ do
      runSourceVm "print dict()\n" `shouldBe` Right ["{}"]

    it "copies a dict" $ do
      runSourceVm "print dict({'a': 1})\n" `shouldBe` Right ["{a: 1}"]

    it "builds from a list of tuples" $ do
      runSourceVm "print dict([(1, 2), (3, 4)])\n" `shouldBe` Right ["{1: 2, 3: 4}"]

    it "builds from a list of lists" $ do
      runSourceVm "print dict([[1, 2], [3, 4]])\n" `shouldBe` Right ["{1: 2, 3: 4}"]

    it "lets the last value win for duplicate keys" $ do
      runSourceVm "print dict([(1, 2), (1, 3)])\n" `shouldBe` Right ["{1: 3}"]

    it "reports value error for an element with too many items" $ do
      runSourceVm "print dict([(1, 2, 3)])\n" `shouldBe` Left "Value error: dict update sequence element has length 3; 2 is required at 1:7"

    it "reports value error for an element with too few items" $ do
      runSourceVm "print dict([(1,)])\n" `shouldBe` Left "Value error: dict update sequence element has length 1; 2 is required at 1:7"

    it "reports type error for a non-pair element" $ do
      runSourceVm "print dict([1])\n" `shouldBe` Left "Type error: dict update sequence element is not a pair at 1:7"

    it "reports type error for non-iterable" $ do
      runSourceVm "print dict(5)\n" `shouldBe` Left "Type error: dict expects dict or list of pairs at 1:7"

    it "reports argument count mismatch" $ do
      runSourceVm "print dict({}, {})\n" `shouldBe` Left "Argument count mismatch when calling dict at 1:7"
